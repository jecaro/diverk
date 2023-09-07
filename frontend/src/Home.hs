{-# LANGUAGE PatternSynonyms #-}

module Home (home) where

import Common.Model
  ( Owner (..),
    Repo (..),
  )
import Common.Route (FrontendRoute (..))
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Reflex.Dom.Core hiding (Error)

home ::
  ( DomBuilder t m,
    PostBuild t m,
    Prerender t m,
    RouteToUrl (R FrontendRoute) m,
    SetRoute t (R FrontendRoute) m
  ) =>
  m ()
home = do
  dynOwner <- fmap MkOwner <$> inputWidget "Owner" "jecaro"
  dynRepo <- fmap MkRepo <$> inputWidget "Repo" "mprisqueeze"
  dyn_ . ffor (zipDyn dynOwner dynRepo) $ \(owner, repo) -> do
    el "div" $ routeLink (MkOwnerAndRepo :/ (owner, (repo, []))) $ text "Go to repo"

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
