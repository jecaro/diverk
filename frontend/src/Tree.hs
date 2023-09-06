{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Tree (tree) where

import Common.Model (Hash (..), Owner, Repo, treeUrl)
import Common.Route (FrontendRoute)
import qualified Common.Route as Route
import Control.Lens
  ( abbreviatedFields,
    makeLensesWith,
    (^.),
    (^..),
    (^?),
    _Unwrapped,
  )
import Control.Monad (forM_)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, values, _String)
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Reflex.Dom.Core

data ObjectKind = Blob | Tree
  deriving (Eq, Show)

data Object = MkObject
  { obPath :: Text,
    obHash :: Hash,
    obKind :: ObjectKind
  }
  deriving (Eq, Show)

makeLensesWith abbreviatedFields ''Object

data Error = ErWrongStatus Word | ErWrongJSON | ErOtherError Text | ErInvalid
  deriving (Eq, Show)

data State = StInitial | StFetching | StOk [Object] | StError Error
  deriving (Eq, Show)

data LocalEvent
  = LoStartRequest
  | LoEndRequest (Either XhrException XhrResponse)

updateState :: LocalEvent -> State -> State
updateState LoStartRequest StInitial = StFetching
updateState (LoEndRequest (Left _)) StFetching =
  StError (ErOtherError "Request error")
updateState (LoEndRequest (Right response)) StFetching = responseToState response
updateState _ _ = StError ErInvalid

responseToState :: XhrResponse -> State
responseToState response =
  case response ^. xhrResponse_status of
    200 -> maybe (StError ErWrongJSON) StOk mbObjects
    code -> StError $ ErWrongStatus code
  where
    mbObjects = do
      json :: JSON.Value <- decodeXhrResponse response
      let objects = json ^.. key "tree" . values
      traverse toObject objects

    toObject :: JSON.Value -> Maybe Object
    toObject v = do
      obPath <- v ^? key "path" . _String
      obHash <- v ^? key "sha" . _String . _Unwrapped
      obKind <-
        v ^? key "type" . _String >>= \case
          "blob" -> Just Blob
          "tree" -> Just Tree
          _ -> Nothing
      pure $ MkObject {..}

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
  Hash ->
  m ()
tree owner repo hash' = do
  evRequest <- (xhrRequest "GET" (treeUrl owner repo hash') def <$) <$> getPostBuild
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
    StOk objects ->
      forM_ objects $ \object -> do
        el "div" $ case object ^. kind of
          Blob -> text $ object ^. path
          Tree ->
            routeLink
              ( Route.MkRepo :/ (owner, (repo, Route.MkTree :/ (object ^. hash)))
              )
              . text
              $ object ^. path
    _ -> blank
