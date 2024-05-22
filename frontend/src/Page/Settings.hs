{-# LANGUAGE RecursiveDo #-}

module Page.Settings (page) where

import Common.Model
  ( Config (..),
    Owner (..),
    Repo (..),
    Token (..),
    darkMode,
    owner,
    repo,
    token,
  )
import Control.Lens (to, (^.), (^?), _Just, _Wrapped)
import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Error)
import Reflex.Extra (onClient)
import qualified Request
import qualified Widget
import qualified Widget.Icon as Icon
import Witherable (catMaybes)
import Prelude hiding (unzip)

page ::
  ( DomBuilder t m,
    Prerender t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m)
  ) =>
  Maybe Config ->
  m (Event t Config)
page mbConfig =
  Widget.card $ do
    rec dyOwner <- fmap MkOwner <$> inputOwner evOwnerValid
        dyRepo <- fmap MkRepo <$> inputRepo (updated dyRepoExists)
        dyToken <- fmap mkToken <$> inputToken (updated dyTokenValid)
        dyDarkMode <- inputDarkMode

        -- The owner request
        let evUserRequest = updated $ Request.users <$> dyToken <*> dyOwner
        evOwnerResponse <- debounceAndRequest evUserRequest
        -- 401 means the token is wrong. In this case we assume the owner
        -- exists. Because the token is wrong, the form cannot be submitted
        -- anyway.
        let evOwnerValid =
              leftmost
                [ -- The owner is valid
                  is200Or401 <$> evOwnerResponse,
                  -- It is currently edited
                  False <$ updated dyOwner
                ]

        -- The repo request
        let evContentRequest =
              updated $
                Request.contents
                  <$> dyToken <*> dyOwner <*> dyRepo <*> pure mempty
        evRepoResponse <- debounceAndRequest evContentRequest
        -- Same remark for 401
        dyRepoExists <-
          holdDyn (isJust mbRepo) $
            leftmost
              [ is200Or401 <$> evRepoResponse,
                False <$ updated dyOwner,
                False <$ updated dyRepo
              ]

        -- The token request
        -- The token is valid:
        -- - if empty
        -- - if the rate limit endpoint returns 200
        let evToken = updated dyToken
            evMaybeTokenRequest = fmap Request.rateLimit <$> evToken
        evTokenResponse <-
          -- dont debounce the request if the token is empty
          fmap (gate (isJust <$> current dyToken))
            . debounceAndRequest
            $ catMaybes evMaybeTokenRequest
        let evTokenValidOrEmpty =
              leftmost
                [ -- Valid non empty token
                  is200 <$> evTokenResponse,
                  -- Empty token
                  isNothing <$> evToken,
                  -- Token currently edited
                  False <$ evToken
                ]
        -- In the initial state, the token is either empty either loaded
        -- from the local storage. In both cases, we assume it is valid.
        dyTokenValid <- holdDyn True evTokenValidOrEmpty

    let dyCanSave = (&&) <$> dyRepoExists <*> dyTokenValid
    evSave <- saveButton dyCanSave

    let beConfig =
          current $
            MkConfig <$> dyOwner
              <*> dyRepo
              <*> dyToken
              <*> dyDarkMode
    pure $ tag beConfig evSave
  where
    inputOwner evValid =
      inputWidget
        MkText
        "Owner"
        True
        "name"
        (fromMaybe "" mbOwner)
        (isJust mbOwner)
        evValid
        Nothing
    inputRepo evValid =
      inputWidget
        MkText
        "Repository"
        True
        "repository"
        (fromMaybe "" mbRepo)
        (isJust mbRepo)
        evValid
        Nothing
    inputToken evValid =
      inputWidget
        MkPassword
        "Token"
        False
        "github_xxx"
        (fromMaybe "" mbToken)
        True
        evValid
        (Just "Needed to access private repositories")

    inputDarkMode =
      elClass "div" "form-control" $
        elClass "label" "label cursor-pointer" $ do
          elClass "span" "label-text" $
            text "Dark mode"
          _inputElement_checked
            <$> inputElement
              ( def
                  & inputElementConfig_initialChecked
                    .~ fromMaybe
                      False
                      mbDarkMode
                  & initialAttributes
                    .~ ( "class" =: "toggle"
                           <> "type" =: "checkbox"
                       )
              )

    saveButton dyEnable = do
      (ev, _) <-
        elDynAttr'
          "button"
          (constDyn ("class" =: buttonClasses) <> (enableAttr <$> dyEnable))
          $ text "Save"
      pure $ domEvent Click ev

    mbOwner = mbConfig ^? _Just . owner . _Wrapped
    mbRepo = mbConfig ^? _Just . repo . _Wrapped
    mbToken = mbConfig ^? _Just . token . _Just . _Wrapped
    mbDarkMode = mbConfig ^? _Just . darkMode

    mkToken "" = Nothing
    mkToken txToken = Just $ MkToken txToken

    debounceAndRequest = onClient . performRequestAsyncWithError <=< debounce 0.5

    is200 = checkStatus (== 200)
    is200Or401 = checkStatus (`elem` [200, 401])

    checkStatus _ (Left _) = False
    checkStatus p (Right response) = response ^. xhrResponse_status . to p

    enableAttr True = mempty
    enableAttr False = "disabled" =: "true"

data InputType = MkPassword | MkText

toText :: InputType -> Text
toText MkPassword = "password"
toText MkText = "text"

inputWidget ::
  (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m) =>
  InputType ->
  Text ->
  Bool ->
  Text ->
  Text ->
  Bool ->
  Event t Bool ->
  Maybe Text ->
  m (Dynamic t Text)
inputWidget inputType label mandatory placeholder initialValue valid evValid mbHelp =
  elClass "div" "form-control w-full" $ do
    elAttr "label" ("class" =: "label" <> "for" =: inputId) $
      elClass "span" "label-text" $
        text inputLabel

    dyInput <- elClass "div" "relative" $ do
      rec dyInput <-
            value
              <$> inputElement
                ( def
                    & inputElementConfig_initialValue .~ initialValue
                    & initialAttributes
                      .~ ( "class" =: inputClasses' valid
                             <> "type" =: toText inputType
                             <> "placeholder" =: placeholder
                             <> "id" =: inputId
                         )
                    & modifyAttributes
                      .~ ( ((=:) "class" . Just . inputClasses' <$> evValid)
                             <> (toggleInputType inputType <$> evPasswordVisible)
                         )
                )
          evPasswordVisible <- elEye inputType
      pure dyInput

    elHelp mbHelp

    pure dyInput
  where
    inputClasses' = inputClasses inputType

    inputId = T.toLower label
    inputLabel = label <> if mandatory then " *" else ""

    toggleInputType MkText _ = mempty
    toggleInputType MkPassword True = "type" =: Just "text"
    toggleInputType MkPassword False = "type" =: Just "password"

    elEye MkText = pure never
    elEye MkPassword = do
      rec ev <- elClass
            "div"
            "absolute inset-y-0 right-0 pr-3 flex items-center"
            $ do
              (e, _) <-
                elDynClass'
                  "span"
                  (eyeClasses <$> dyPasswordVisible)
                  blank
              pure $ domEvent Click e
          dyPasswordVisible <- toggle False ev
      pure $ updated dyPasswordVisible

    eyeClasses = T.unwords . ([Icon.solid, "cursor-pointer"] <>) . pure . eyeIcon

    eyeIcon True = Icon.eyeSlashName
    eyeIcon False = Icon.eyeName

    elHelp Nothing = pure ()
    elHelp (Just help) =
      elClass "label" "label" $
        elClass "span" "label-text-alt" $
          text help

inputClasses :: InputType -> Bool -> Text
inputClasses inputType valid =
  T.unwords $
    ["input", "input-bordered", "w-full"]
      <> validClasses valid
      <> inputTypeClasses inputType
  where
    validClasses True = mempty
    validClasses False = ["input-error"]
    -- Make room for the eye icon
    inputTypeClasses MkPassword = ["pr-10"]
    inputTypeClasses MkText = mempty

buttonClasses :: Text
buttonClasses = T.unwords ["w-full", "btn", "btn-primary"]
