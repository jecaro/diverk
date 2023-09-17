{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Browse (browse) where

import Common.Model (Config (..))
import Common.Route (FrontendRoute (..))
import Control.Lens
  ( abbreviatedFields,
    makeLensesWith,
    preview,
    to,
    toListOf,
    (^.),
    (^?),
    _last,
  )
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import Data.Bifunctor (Bifunctor (first))
import Data.Either.Extra (maybeToEither)
import Data.List (inits)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base64 (decodeBase64)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Reflex.Dom.Core
import Reflex.Extra (onClient)
import Request (contentsRequest)

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

browse ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    MonadFix m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m
  ) =>
  Config ->
  [Text] ->
  m ()
browse config path' = do
  navbar path'
  contentWidget config path'

navbar ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    Prerender t m
  ) =>
  [Text] ->
  m ()
navbar path' = do
  elAttr "nav" ("class" =: "sticky shadow-md top-0 flex flex-col p-4 bg-white") $
    elAttr "ol" ("class" =: "flex gap-x-4  w-full") $ do
      forM_ (inits path') $ \intermediatePath ->
        el "li" $
          routeLink (MkBrowse :/ intermediatePath) $ homeOrText intermediatePath
      -- spacer
      elAttr "li" ("class" =: "grow") blank
      el "li" $ routeLink (MkConfiguration :/ ()) gear
  where
    house = elAttr "i" ("class" =: "fa-solid fa-house") blank
    gear = elAttr "i" ("class" =: "fa-solid fa-gear") blank
    homeOrText [] = house
    homeOrText [x] = text x
    homeOrText (_ : xs) = homeOrText xs

contentWidget ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender t m
  ) =>
  Config ->
  [Text] ->
  m ()
contentWidget MkConfig {..} path' = do
  evRequest <-
    (contentsRequest coToken coOwner coRepo path' <$) <$> getPostBuild
  evResponse <- onClient $ performRequestAsyncWithError evRequest

  dynState <-
    foldDyn updateState StInitial $
      leftmost
        [LoStartRequest <$ evRequest, LoEndRequest <$> evResponse]

  elAttr "div" ("class" =: "flex flex-col gap-4 p-4 overflow-auto") $
    dyn_ . ffor dynState $ \case
      StError err -> el "div" . text . T.pack $ show err
      StTree objects ->
        forM_ objects $ \object ->
          el "div" $
            routeLink (MkBrowse :/ object ^. path)
              . text
              . fromMaybe "/"
              $ object ^? path . _last
      StBlob object ->
        elAttr "div" ("class" =: "whitespace-pre-wrap") $
          text $ object ^. content
      _ -> blank
