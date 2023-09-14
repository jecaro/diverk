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
  let mbOwner = mbConfig ^? _Just . owner . _Wrapped
      mbRepo = mbConfig ^? _Just . repo . _Wrapped
      mbToken = mbConfig ^? _Just . token . _Just . _Wrapped

  dyOwner <- fmap MkOwner <$> inputWidget "Owner" (fromMaybe "" mbOwner)
  dyRepo <- fmap MkRepo <$> inputWidget "Repo" (fromMaybe "" mbRepo)
  dyToken <- fmap mkToken <$> inputWidget "Token" (fromMaybe "" mbToken)

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

  evGo <- do
    (ev, _) <-
      elDynAttr'
        "button"
        (constDyn ("class" =: buttonClasses) <> (enableAttr <$> dyRepoExists))
        $ text "Go"
    pure $ domEvent Click ev

  let beConfig = current $ MkConfig <$> dyOwner <*> dyRepo <*> dyToken
  pure $ tag beConfig evGo

mkToken :: Text -> Maybe Token
mkToken "" = Nothing
mkToken txToken = Just $ MkToken txToken

enableAttr :: Bool -> Map Text Text
enableAttr True = mempty
enableAttr False = "disabled" =: "true"

is200 :: Either XhrException XhrResponse -> Bool
is200 (Left _) = False
is200 (Right response) = response ^. xhrResponse_status == 200

inputWidget :: (DomBuilder t m) => Text -> Text -> m (Dynamic t Text)
inputWidget label initialValue =
  el "div" $ do
    elAttr
      "label"
      ( "class" =: "block mb-2 text-sm font-medium text-gray-900"
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
    [ "bg-blue-500",
      "hover:bg-blue-600",
      "text-white",
      "px-4",
      "py-2",
      "rounded",
      "text-center",
      "disabled:opacity-50"
    ]
