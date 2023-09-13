module Configuration (configuration) where

import Common.Model (Owner (..), Repo (..))
import Control.Lens ((^.))
import Data.List.NonEmpty (unzip)
import Data.Map (Map)
import Data.Maybe (isJust)
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
  Maybe (Owner, Repo) ->
  m (Event t (Owner, Repo))
configuration mbConfig = do
  let (mbOwner, mbRepo) = unzip mbConfig

  dyOwner <- fmap MkOwner <$> inputWidget "Owner" (maybe "" unOwner mbOwner)
  dyRepo <- fmap MkRepo <$> inputWidget "Repo" (maybe "" unRepo mbRepo)

  evUserResponse <-
    onClient . performRequestAsyncWithError $ usersRequest <$> updated dyOwner
  beUserExists <- hold (isJust mbConfig) $ is200 <$> evUserResponse

  let evRepoRequest =
        gate beUserExists
          . updated
          $ contentsRequest <$> dyOwner <*> dyRepo <*> pure []
  evRepoResponse <- onClient $ performRequestAsyncWithError evRepoRequest
  dyRepoExists <- holdDyn (isJust mbConfig) $ is200 <$> evRepoResponse

  evGo <- do
    (ev, _) <-
      elDynAttr'
        "button"
        (constDyn ("class" =: buttonClasses) <> (enableAttr <$> dyRepoExists))
        $ text "Go"
    pure $ domEvent Click ev

  let beOwnerAndRepo = current $ zipDyn dyOwner dyRepo
  pure $ tag beOwnerAndRepo evGo

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
