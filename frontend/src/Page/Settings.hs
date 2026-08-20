{-# LANGUAGE RecursiveDo #-}

module Page.Settings (page) where

import Control.Lens ((^.), (^?))
import qualified Control.Lens as Lens
import Control.Monad ((<=<))
import qualified Control.Monad as Monad
import qualified Control.Monad.Fix as MonadFix
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Model
import Reflex.Dom.Core hiding (Error)
import Reflex.Extra (onClient)
import qualified Request
import qualified Theme
import qualified Widget
import qualified Widget.Icon as Icon
import qualified Witherable
import Prelude hiding (unzip)

page ::
  ( DomBuilder t m,
    Prerender t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix.MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO.MonadIO (Performable m)
  ) =>
  Maybe Model.Config ->
  m (Event t Model.Config)
page mbConfig =
  elAttr "div" ("style" =: "padding-top: env(safe-area-inset-top)") $
    Widget.card $ do
      rec dyOwner <- fmap Model.MkOwner <$> inputOwner evOwnerValid
          dyRepo <- fmap Model.MkRepo <$> inputRepo (updated dyRepoExists)
          dyToken <- fmap mkToken <$> inputToken (updated dyTokenValid)
          dyDarkMode <- inputDarkMode

          Monad.void . Theme.setDarkModeOn $ updated dyDarkMode

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
                    <$> dyToken
                    <*> dyOwner
                    <*> dyRepo
                    <*> pure mempty
          evRepoResponse <- debounceAndRequest evContentRequest
          -- Same remark for 401
          dyRepoExists <-
            holdDyn (Maybe.isJust mbRepo) $
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
            fmap (gate (Maybe.isJust <$> current dyToken))
              . debounceAndRequest
              $ Witherable.catMaybes evMaybeTokenRequest
          let evTokenValidOrEmpty =
                leftmost
                  [ -- Valid non empty token
                    is200 <$> evTokenResponse,
                    -- Empty token
                    Maybe.isNothing <$> evToken,
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
              Model.MkConfig
                <$> dyOwner
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
        (Maybe.fromMaybe "" mbOwner)
        (Maybe.isJust mbOwner)
        evValid
        Nothing
    inputRepo evValid =
      inputWidget
        MkText
        "Repository"
        True
        "repository"
        (Maybe.fromMaybe "" mbRepo)
        (Maybe.isJust mbRepo)
        evValid
        Nothing
    inputToken evValid =
      inputWidget
        MkPassword
        "Token"
        False
        "github_xxx"
        (Maybe.fromMaybe "" mbToken)
        True
        evValid
        (Just "Needed to access private repositories")

    inputDarkMode = do
      evSystemDarkMode <- Theme.getSystemDarkModeEvent
      let darkModeFromConfig = Maybe.fromMaybe False mbDarkMode
          evSystemDarkModeWhenNotSet
            -- Dont default with the system when we have a value in the config
            | Maybe.isJust mbDarkMode = never
            | otherwise = evSystemDarkMode
      elClass "div" "form-control" $
        elClass "label" "label cursor-pointer" $ do
          elClass "span" "label-text" $
            text "Dark mode"
          _inputElement_checked
            <$> inputElement
              ( def
                  & inputElementConfig_initialChecked
                  .~ darkModeFromConfig
                  & inputElementConfig_setChecked
                  .~ evSystemDarkModeWhenNotSet
                  & initialAttributes
                  .~ ("class" =: "toggle" <> "type" =: "checkbox")
              )

    saveButton dyEnable = do
      (ev, _) <-
        elDynAttr'
          "button"
          (constDyn ("class" =: buttonClasses) <> (enableAttr <$> dyEnable))
          $ text "Save"
      pure $ domEvent Click ev

    mbOwner = mbConfig ^? Lens._Just . Model.owner . Lens._Wrapped
    mbRepo = mbConfig ^? Lens._Just . Model.repo . Lens._Wrapped
    mbToken = mbConfig ^? Lens._Just . Model.token . Lens._Just . Lens._Wrapped
    mbDarkMode = mbConfig ^? Lens._Just . Model.darkMode

    mkToken "" = Nothing
    mkToken txToken = Just $ Model.MkToken txToken

    debounceAndRequest = onClient . performRequestAsyncWithError <=< debounce 0.5

    is200 = checkStatus (== 200)
    is200Or401 = checkStatus (`elem` [200, 401])

    checkStatus _ (Left _) = False
    checkStatus p (Right response) = response ^. xhrResponse_status . Lens.to p

    enableAttr True = mempty
    enableAttr False = "disabled" =: "true"

data InputType = MkPassword | MkText

toText :: InputType -> Text.Text
toText MkPassword = "password"
toText MkText = "text"

inputWidget ::
  (DomBuilder t m, MonadHold t m, MonadFix.MonadFix m, PostBuild t m) =>
  InputType ->
  Text.Text ->
  Bool ->
  Text.Text ->
  Text.Text ->
  Bool ->
  Event t Bool ->
  Maybe Text.Text ->
  m (Dynamic t Text.Text)
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
                    & inputElementConfig_initialValue
                    .~ initialValue
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

    inputId = Text.toLower label
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

    eyeClasses =
      Text.unwords . ([Icon.solid, "cursor-pointer"] <>) . pure . eyeIcon

    eyeIcon True = Icon.eyeSlashName
    eyeIcon False = Icon.eyeName

    elHelp Nothing = pure ()
    elHelp (Just help) =
      elClass "label" "label" $
        elClass "span" "label-text-alt" $
          text help

inputClasses :: InputType -> Bool -> Text.Text
inputClasses inputType valid =
  Text.unwords $
    ["input", "input-bordered", "w-full"]
      <> validClasses valid
      <> inputTypeClasses inputType
  where
    validClasses True = mempty
    validClasses False = ["input-error"]
    -- Make room for the eye icon
    inputTypeClasses MkPassword = ["pr-10"]
    inputTypeClasses MkText = mempty

buttonClasses :: Text.Text
buttonClasses = Text.unwords ["w-full", "btn", "btn-primary"]
