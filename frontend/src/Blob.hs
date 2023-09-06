module Blob (blob) where

import Common.Model (Hash, Owner, Repo, blobUrl)
import Control.Lens ((^.), (^?))
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, _String)
import Data.Bifunctor (Bifunctor (first))
import Data.Either.Extra (maybeToEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding.Base64 (decodeBase64)
import Reflex.Dom.Core

data Error = ErStatus Word | ErJSON | ErBase64 Text | ErRequest | ErInvalid
  deriving (Eq, Show)

data State = StInitial | StFetching | StOk Text | StError Error
  deriving (Eq, Show)

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
    200 -> either StError StOk eiContentDecoded
    code -> StError $ ErStatus code
  where
    eiContentDecoded = do
      content <- maybeToEither ErJSON parsedJSON
      first ErBase64 . decodeBase64 $ withoutEOL content
    parsedJSON = do
      json :: JSON.Value <- decodeXhrResponse response
      json ^? key "content" . _String
    -- The GitHub API pads the text with newlines every 60 characters
    withoutEOL = T.filter (/= '\n')

blob ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    MonadHold t m,
    MonadFix m
  ) =>
  Owner ->
  Repo ->
  Hash ->
  m ()
blob owner repo hash' = do
  evRequest <- (xhrRequest "GET" (blobUrl owner repo hash') def <$) <$> getPostBuild
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
    StOk content ->
      elAttr "div" ("class" =: "whitespace-pre") $
        text content
    StError err -> el "div" . text . T.pack $ show err
    _ -> blank
