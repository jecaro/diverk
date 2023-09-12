module Home (home) where

import Common.Model
  ( Owner (..),
    Repo (..),
  )
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core hiding (Error)

home :: (DomBuilder t m) => Maybe Owner -> Maybe Repo -> m (Event t (Owner, Repo))
home mbOwner mbRepo = do
  dyOwner <- fmap MkOwner <$> inputWidget "Owner" (maybe "" unOwner mbOwner)
  dyRepo <- fmap MkRepo <$> inputWidget "Repo" (maybe "" unRepo mbRepo)
  evGo <- button "Go"
  let dyOwnerAndRepo = zipDyn dyOwner dyRepo

  pure $ tag (current dyOwnerAndRepo) evGo

inputWidget :: (DomBuilder t m) => Text -> Text -> m (Dynamic t Text)
inputWidget label initialValue =
  el "div" $ do
    elAttr
      "label"
      ( "class" =: "block mb-2 text-sm font-medium text-gray-900 dark:text-white"
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
