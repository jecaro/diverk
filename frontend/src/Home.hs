{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Home (home) where

import Common.Model
  ( Branch (..),
    GitTree (..),
    Hash (..),
    Owner (..),
    Repo (..),
    repoUrl,
  )
import Common.Route as Route (FrontendRoute)
import qualified Common.Route as Route
import Control.Lens (preview, (^.), _Unwrapped)
import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, _String)
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLinkAttr)
import Reflex.Dom.Core hiding (Error)

data Error = ErWrongStatus Word | ErWrongJSON | ErOtherError Text | ErInvalid
  deriving (Eq, Show)

data State
  = StNotChecked
  | StTrying
  | StOk Hash
  | StError Error
  deriving (Eq, Show)

data LocalEvent
  = LoClicked
  | LoEndRequest (Either XhrException XhrResponse)

home ::
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    Prerender t m,
    MonadFix m,
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m
  ) =>
  m ()
home = do
  rec dynGitTree <- repoInput
      evButtonClick <- buttonThenLink $ zipDyn dynState dynGitTree
      evRequestPerformed <- requestPerformed (repoUrl <$> dynGitTree) evButtonClick

      -- We merge all the events in the page
      let events =
            leftmost
              [ LoClicked <$ evButtonClick,
                LoEndRequest <$> evRequestPerformed
              ]
      -- To maintain the state
      dynState <- foldDyn updateState StNotChecked events
      message dynState
  pure ()

message :: (DomBuilder t m, PostBuild t m) => Dynamic t State -> m ()
message dynState =
  dyn_ . ffor dynState $ \case
    StError e -> el "div" . text $ T.pack (show e)
    StOk (MkHash hash) -> el "div" $ text hash
    _ -> blank

-- The state machine
updateState :: LocalEvent -> State -> State
updateState LoClicked StNotChecked = StTrying
updateState LoClicked (StError _) = StTrying
updateState (LoEndRequest (Left _)) StTrying = StError (ErOtherError "Request error")
updateState (LoEndRequest (Right response)) StTrying = responseToState response
updateState _ _ = StError ErInvalid

responseToState :: XhrResponse -> State
responseToState response =
  case response ^. xhrResponse_status of
    200 -> maybe (StError ErWrongJSON) StOk mbTree
    code -> StError (ErWrongStatus code)
  where
    mbTree = preview tree =<< (decodeXhrResponse response :: Maybe JSON.Value)
    tree =
      key "commit" . key "commit" . key "tree" . key "sha" . _String . _Unwrapped

requestPerformed ::
  (Applicative f, Prerender t f) =>
  Dynamic t Text ->
  Event t a ->
  f (Event t (Either XhrException XhrResponse))
requestPerformed dynUrl event = do
  -- That dynamic event is triggered each time the url change
  let dynReq = xhrRequest "GET" <$> dynUrl <*> pure def
      -- Here we make it so when the click event is fired we start the request
      evStartRequest = tag (current dynReq) event

  switchDyn
    <$> prerender
      (pure never)
      (performRequestAsyncWithError evStartRequest)

repoInput :: (DomBuilder t m) => m (Dynamic t GitTree)
repoInput = do
  dynOwner <- fmap MkOwner <$> inputWidget "Owner" "jecaro"
  dynRepo <- fmap MkRepo <$> inputWidget "Repo" "mprisqueeze"
  dynBranch <- fmap MkBranch <$> inputWidget "Branch" "main"
  pure $ MkGitTree <$> dynOwner <*> dynRepo <*> dynBranch

buttonThenLink ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    Prerender t m
  ) =>
  Dynamic t (State, GitTree) ->
  m (Event t ())
buttonThenLink dynState = do
  evEvent <-
    dyn . ffor dynState $ \case
      (StOk hash, MkGitTree {..}) -> do
        routeLinkAttr
          ("class" =: "block")
          (Route.MkRepo :/ (reOwner, (reRepo, (reBranch, Route.MkTree :/ hash))))
          . elAttr "button" ("class" =: buttonClasses)
          $ text "Go"
        pure never
      (state, _) -> button' . text $ toText state
  switchHold never evEvent
  where
    toText StTrying = "Checking..."
    toText _ = "Check"

button' :: DomBuilder t m => m () -> m (Event t ())
button' child = do
  (e, _) <- elAttr' "button" ("class" =: buttonClasses) child
  pure $ domEvent Click e

inputWidget :: (DomBuilder t m) => Text -> Text -> m (Dynamic t Text)
inputWidget label initialValue =
  el "div" $ do
    elAttr
      "label"
      ( "for" =: "url"
          <> "class" =: "block mb-2 text-sm font-medium text-gray-900 dark:text-white"
      )
      $ text label
    value
      <$> inputElement
        ( def
            & inputElementConfig_initialValue .~ initialValue
            & initialAttributes .~ ("class" =: inputClasses)
        )

inputClasses :: Text
inputClasses =
  T.unwords
    [ "block",
      "w-full",
      "bg-gray-50",
      "border",
      "border-gray-300",
      "rounded-lg",
      "focus:ring-blue-500",
      "focus:border-blue-500",
      "p-2.5"
    ]

buttonClasses :: Text
buttonClasses =
  T.unwords
    [ "text-white",
      "bg-blue-700",
      "hover:bg-blue-800",
      "focus:ring-4",
      "focus:ring-blue-300",
      "font-medium",
      "rounded-lg",
      "text-sm",
      "px-5",
      "py-2.5",
      "focus:outline-none"
    ]
