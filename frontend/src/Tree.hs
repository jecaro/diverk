{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Tree (tree) where

import Common.Model (Owner, Repo, contentsURL)
import Common.Route (FrontendRoute (..))
import Control.Lens
  ( abbreviatedFields,
    makeLensesWith,
    preview,
    to,
    toListOf,
    (^.),
  )
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import Data.Bifunctor (Bifunctor (first))
import Data.Either.Extra (maybeToEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base64 (decodeBase64)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Reflex.Dom.Core

newtype TreeElt = MkTreeElt
  { trPath :: [Text]
  }
  deriving stock (Eq, Show)

data Blob = MkBlob
  { blPath :: [Text],
    blContent :: Text
  }
  deriving stock (Eq, Show)

makeLensesWith abbreviatedFields ''Blob
makeLensesWith abbreviatedFields ''TreeElt

data Error = ErStatus Word | ErJSON | ErBase64 Text | ErRequest | ErInvalid
  deriving stock (Eq, Show)

data State
  = StInitial
  | StFetching
  | StTree [TreeElt]
  | StBlob Blob
  | StError Error
  deriving stock (Eq, Show)

data LocalEvent
  = LoStartRequest
  | LoEndRequest (Either XhrException XhrResponse)

updateState :: LocalEvent -> State -> State
updateState LoStartRequest StInitial = StFetching
updateState (LoEndRequest (Left _)) StFetching = StError ErRequest
updateState (LoEndRequest (Right response)) StFetching = responseToState response
updateState _ _ = StError ErInvalid

responseToState :: XhrResponse -> State
responseToState response =
  case response ^. xhrResponse_status of
    200 -> either StError (either StTree StBlob) eiObjects
    code -> StError $ ErStatus code
  where
    eiObjects :: Either Error (Either [TreeElt] Blob)
    eiObjects = do
      v <- maybeToEither ErJSON $ decodeXhrResponse response
      case v of
        JSON.Object _ -> Right <$> toBlob v
        JSON.Array _ -> Left <$> toTreeElts v
        _ -> Left ErJSON

    toBlob :: JSON.Value -> Either Error Blob
    toBlob v = do
      blPath <- maybeToEither ErJSON $ parsePath v
      rawContent <- maybeToEither ErJSON $ parseContent v
      blContent <- first ErBase64 $ decodeBase64 rawContent
      pure $ MkBlob {..}

    toTreeElt :: JSON.Value -> Maybe TreeElt
    toTreeElt = fmap MkTreeElt . parsePath

    toTreeElts :: JSON.Value -> Either Error [TreeElt]
    toTreeElts = maybeToEither ErJSON . traverse toTreeElt . toListOf values

    parseContent = preview $ key "content" . _String . to withoutEOL
    -- The GitHub API pads the text with newlines every 60 characters
    withoutEOL = T.filter (/= '\n')
    parsePath = preview $ key "path" . _String . to splitPath
    splitPath = T.split (== '/')

tree ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    MonadFix m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m
  ) =>
  Owner ->
  Repo ->
  [Text] ->
  m ()
tree owner repo path' = do
  evRequest <-
    (xhrRequest "GET" (contentsURL owner repo path') def <$) <$> getPostBuild
  evResponse <-
    switchDyn
      <$> prerender
        (pure never)
        (performRequestAsyncWithError evRequest)

  dynState <-
    foldDyn updateState StInitial $
      leftmost
        [LoStartRequest <$ evRequest, LoEndRequest <$> evResponse]

  dyn_ . ffor dynState $ \case
    StError err -> el "div" . text . T.pack $ show err
    StTree objects ->
      forM_ objects $ \object ->
        el "div" $
          routeLink (MkBrowse :/ object ^. path)
            . text
            $ T.intercalate "/" $ object ^. path
    StBlob object ->
      elAttr "div" ("class" =: "whitespace-pre") $
        text $ object ^. content
    _ -> blank
