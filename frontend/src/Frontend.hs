{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend (frontend) where

import Common.Route (FrontendRoute (..))
import Control.Lens (makeLenses, preview, (^.))
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, _String)
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLinkAttr)
import Reflex.Dom.Core hiding (Error)

data Repo = Repo
  { _reOwner :: Text,
    _reName :: Text,
    _reBranch :: Text
  }
  deriving (Eq, Show)

makeLenses ''Repo

data Error = ErWrongStatus Word | ErWrongJSON | ErOtherError Text | ErInvalid
  deriving (Eq, Show)

data State
  = StNotChecked
  | StTrying
  | StOk Text
  | StError Error
  deriving (Eq, Show)

data LocalEvent
  = LoClicked
  | LoEndRequest (Either XhrException XhrResponse)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Diverk"
        elAttr
          "link"
          ( "href" =: $(static "css/styles.css")
              <> "type" =: "text/css"
              <> "rel" =: "stylesheet"
          )
          blank,
      _frontend_body = do
        elAttr "div" ("class" =: "mt-4 mb-4 mr-4 ml-4 space-y-4") $ do
          rec dynUrl <- fmap url <$> repoInput
              evButtonClick <- buttonThenLink dynState
              evRequestPerformed <- requestPerformed dynUrl evButtonClick

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
    }

message :: (DomBuilder t m, PostBuild t m) => Dynamic t State -> m ()
message dynState =
  dyn_ . ffor dynState $ \case
    StError e -> el "div" . text $ T.pack (show e)
    StOk tree -> el "div" $ text tree
    _ -> blank

url :: Repo -> Text
url repo =
  T.intercalate
    "/"
    [ "https://api.github.com/repos",
      repo ^. reOwner,
      repo ^. reName,
      "branches",
      repo ^. reBranch
    ]

-- The state machine
updateState :: LocalEvent -> State -> State
updateState LoClicked StNotChecked = StTrying
updateState LoClicked (StError _) = StTrying
updateState (LoEndRequest (Left _)) StTrying = StError (ErOtherError "Error")
updateState (LoEndRequest (Right response)) StTrying = responseToState response
updateState _ _ = StError ErInvalid

responseToState :: XhrResponse -> State
responseToState response =
  case response ^. xhrResponse_status of
    200 -> maybe (StError ErWrongJSON) StOk mbTree
    code -> StError (ErWrongStatus code)
  where
    mbTree = preview tree =<< (decodeXhrResponse response :: Maybe JSON.Value)
    tree = key "commit" . key "commit" . key "tree" . key "sha" . _String

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

repoInput :: (DomBuilder t m) => m (Dynamic t Repo)
repoInput = do
  dynOwner <- inputWidget "Owner" "jecaro"
  dynRepo <- inputWidget "Repo" "mprisqueeze"
  dynBranch <- inputWidget "Branch" "main"
  pure $ Repo <$> dynOwner <*> dynRepo <*> dynBranch

buttonThenLink ::
  ( RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m,
    DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    Prerender t m
  ) =>
  Dynamic t State ->
  m (Event t ())
buttonThenLink dynState = do
  evEvent <-
    dyn . ffor dynState $ \case
      StOk _ -> do
        routeLinkAttr ("class" =: "block") (FrontendRoute_Main :/ ())
          . elAttr "button" ("class" =: buttonClasses)
          $ text "Go"
        pure never
      _ -> button' $ dynText (toText <$> dynState)
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
