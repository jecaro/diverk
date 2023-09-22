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
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Error)
import Reflex.Extra (onClient)
import Request (contentsRequest, usersRequest)
import Prelude hiding (unzip)

configuration ::
  ( DomBuilder t m,
    Prerender t m,
    MonadHold t m,
    PostBuild t m
  ) =>
  Maybe Config ->
  m (Event t Config)
configuration mbConfig = do
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
        dyOwner <- fmap MkOwner <$> inputWidget "text" "Owner" "name" owner'
        dyRepo <-
          fmap MkRepo <$> inputWidget "text" "Repository" "repository" repo'
        dyToken <-
          fmap mkToken <$> inputWidget "password" "Token" "github_xxx" token'

        evUserResponse <-
          onClient . performRequestAsyncWithError . updated $
            usersRequest <$> dyToken <*> dyOwner
        beUserExists <- hold (isJust mbConfig) $ is200 <$> evUserResponse

        let evRepoRequest =
              gate beUserExists
                . updated
                $ contentsRequest <$> dyToken <*> dyOwner <*> dyRepo <*> pure []
        evRepoResponse <- onClient $ performRequestAsyncWithError evRepoRequest
        dyRepoExists <- holdDyn (isJust mbConfig) $ is200 <$> evRepoResponse

        evSave <- do
          (ev, _) <-
            elDynAttr'
              "button"
              ( constDyn ("class" =: buttonClasses)
                  <> (enableAttr <$> dyRepoExists)
              )
              $ text "Save"
          pure $ domEvent Click ev

        let beConfig = current $ MkConfig <$> dyOwner <*> dyRepo <*> dyToken
        pure $ tag beConfig evSave
  where
    owner' = withDefault $ mbConfig ^? _Just . owner . _Wrapped
    repo' = withDefault $ mbConfig ^? _Just . repo . _Wrapped
    token' = withDefault $ mbConfig ^? _Just . token . _Just . _Wrapped
    withDefault = fromMaybe ""

mkToken :: Text -> Maybe Token
mkToken "" = Nothing
mkToken txToken = Just $ MkToken txToken

enableAttr :: Bool -> Map Text Text
enableAttr True = mempty
enableAttr False = "disabled" =: "true"

is200 :: Either XhrException XhrResponse -> Bool
is200 (Left _) = False
is200 (Right response) = response ^. xhrResponse_status == 200

inputWidget :: (DomBuilder t m) => Text -> Text -> Text -> Text -> m (Dynamic t Text)
inputWidget type_ label placeholder initialValue =
  el "div" $ do
    elAttr "label" ("class" =: "block mb-2 text-sm text-gray-900") $
      text label
    value
      <$> inputElement
        ( def
            & inputElementConfig_initialValue .~ initialValue
            & initialAttributes
              .~ ( "class" =: inputClasses
                     <> "placeholder" =: placeholder
                     <> "type" =: type_
                 )
        )

inputClasses :: Text
inputClasses =
  T.unwords
    [ "bg-gray-50",
      "border",
      "border-gray-300",
      "text-gray-900",
      "rounded-lg",
      "focus:ring-blue-600",
      "focus:border-blue-600",
      "block",
      "w-full",
      "p-2.5"
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
