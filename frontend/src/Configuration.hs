{-# LANGUAGE RecursiveDo #-}

module Configuration (configuration) where

import Common.Model
  ( Config (..),
    Owner (..),
    Repo (..),
    Token (..),
    owner,
    repo,
    token,
  )
import Control.Lens ((^.), (^?), _Just, _Wrapped)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Error)
import Reflex.Extra (onClient)
import Request (contentsRequest, rateLimitRequest, usersRequest)
import Witherable (catMaybes)
import Prelude hiding (unzip)

configuration ::
  ( DomBuilder t m,
    Prerender t m,
    MonadHold t m,
    PostBuild t m,
    MonadFix m
  ) =>
  Maybe Config ->
  m (Event t Config)
configuration mbConfig =
  elAttr "div" ("class" =: "flex items-start md:h-screen md:pt-[20vh]") $
    elAttr
      "div"
      ( "class"
          =: T.unwords
            [ "flex",
              "flex-col",
              "md:rounded-lg",
              "md:max-w-md",
              "md:shadow",
              "w-screen",
              "w-full",
              "mx-auto",
              "gap-4",
              "p-4"
            ]
      )
      $ do
        rec dyOwner <- fmap MkOwner <$> inputOwner evOwnerExists
            dyRepo <- fmap MkRepo <$> inputRepo evRepoExists
            dyToken <- fmap mkToken <$> inputToken (updated dyTokenValid)

            -- the owner request, gated by the token being valid
            evOwnerResponse <-
              onClient . performRequestAsyncWithError
                . gate (current dyTokenValid)
                . updated
                $ usersRequest <$> dyToken <*> dyOwner
            -- 401 means the token is wrong. In this case we assume the owner
            -- exists. Because the token is wrong, the form cannot be submitted
            -- anyway.
            let evOwnerExists = is200Or401 <$> evOwnerResponse

            -- the repo request, gated by the owner existing
            beOwnerExists <- hold (isJust mbConfig) evOwnerExists
            let evRepoRequest =
                  gate beOwnerExists
                    . updated
                    $ contentsRequest <$> dyToken <*> dyOwner <*> dyRepo <*> pure []
            evRepoResponse <- onClient $ performRequestAsyncWithError evRepoRequest
            -- same remark for 401
            let evRepoExists = is200Or401 <$> evRepoResponse
                dyTokenRequest = fmap rateLimitRequest <$> dyToken
                evMaybeTokenRequest = updated dyTokenRequest
                evTokenRequest = catMaybes evMaybeTokenRequest
            dyRepoExists <- holdDyn (isJust mbRepo) $ is200 <$> evRepoResponse

            -- the token request valid if empty or if it returns 200 on the
            -- rate limit endpoint
            evTokenResponse <- onClient $ performRequestAsyncWithError evTokenRequest
            let evTokenValidFromResponse = is200 <$> evTokenResponse
                evTokenEmpty = isNothing <$> evMaybeTokenRequest
                evFinal = leftmost [evTokenValidFromResponse, evTokenEmpty]
            dyTokenValid <- holdDyn True evFinal

        let dyCanSave = (&&) <$> dyRepoExists <*> dyTokenValid
        evSave <- saveButton dyCanSave

        let beConfig = current $ MkConfig <$> dyOwner <*> dyRepo <*> dyToken
        pure $ tag beConfig evSave
  where
    inputOwner evOwnerExists =
      inputWidget
        "text"
        "Owner *"
        "name"
        (fromMaybe "" mbOwner)
        (isJust mbOwner)
        evOwnerExists
        Nothing
    inputRepo evRepoExists =
      inputWidget
        "text"
        "Repository *"
        "repository"
        (fromMaybe "" mbRepo)
        (isJust mbRepo)
        evRepoExists
        Nothing
    inputToken evTokenValid =
      inputWidget
        "password"
        "Token"
        "github_xxx"
        (fromMaybe "" mbToken)
        True
        evTokenValid
        (Just "Needed to access private repositories")
    saveButton dyCanSave = do
      (ev, _) <-
        elDynAttr'
          "button"
          (constDyn ("class" =: buttonClasses) <> (enableAttr <$> dyCanSave))
          $ text "Save"
      pure $ domEvent Click ev

    mbOwner = mbConfig ^? _Just . owner . _Wrapped
    mbRepo = mbConfig ^? _Just . repo . _Wrapped
    mbToken = mbConfig ^? _Just . token . _Just . _Wrapped

    is200 = checkStatus (== 200)
    is200Or401 = checkStatus (\status -> status == 200 || status == 401)

    checkStatus _ (Left _) = False
    checkStatus p (Right response) = p $ response ^. xhrResponse_status

    mkToken "" = Nothing
    mkToken txToken = Just $ MkToken txToken

    enableAttr True = mempty
    enableAttr False = "disabled" =: "true"

inputWidget ::
  (DomBuilder t m) =>
  Text ->
  Text ->
  Text ->
  Text ->
  Bool ->
  Event t Bool ->
  Maybe Text ->
  m (Dynamic t Text)
inputWidget type_ label placeholder initialValue valid evValid mbHelp =
  el "div" $ do
    elAttr "label" ("class" =: "block mb-2 text-sm text-gray-900") $ do
      text label
    dyInput <-
      value
        <$> inputElement
          ( def
              & inputElementConfig_initialValue .~ initialValue
              & initialAttributes
                .~ ( "class" =: inputClasses valid
                       <> "placeholder" =: placeholder
                       <> "type" =: type_
                   )
              & modifyAttributes
                .~ ((=:) "class" . Just . inputClasses <$> evValid)
          )
    case mbHelp of
      Nothing -> pure ()
      Just help ->
        elAttr "p" ("class" =: "mt-2 text-sm text-gray-500") $ text help
    pure dyInput

inputClasses :: Bool -> Text
inputClasses valid =
  T.unwords $
    [ "bg-gray-50",
      "border",
      "rounded-lg",
      "block",
      "w-full",
      "p-2.5"
    ]
      <> if valid
        then
          [ "border-gray-300",
            "text-gray-900",
            "focus:ring-blue-600",
            "focus:border-blue-600"
          ]
        else
          [ "border-red-300",
            "text-red-900",
            "focus:ring-red-600",
            "focus:border-red-600"
          ]

buttonClasses :: Text
buttonClasses =
  T.unwords
    [ "w-full",
      "text-white",
      "bg-blue-600",
      "focus:ring-4",
      "focus:outline-none",
      "focus:ring-blue-300",
      "font-medium",
      "rounded-lg",
      "text-sm",
      "px-5",
      "py-2.5",
      "text-center",
      "disabled:opacity-50"
    ]
