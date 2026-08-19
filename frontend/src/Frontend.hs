{-# LANGUAGE RecursiveDo #-}

module Frontend (frontendHead, frontendBody) where

import Control.Lens (preview, to, _Just)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import LocalStorage (load, save)
import Model (Config (..), darkMode)
import qualified Page.About as About
import qualified Page.Browse as Browse
import qualified Page.Search as Search
import qualified Page.Settings as Settings
import Reflex.Dom.Core hiding (Home, Search)
import qualified Route
import Theme (setDarkModeOn)
import Witherable (catMaybes)

data State
  = -- | The initial state: before the config is loaded from the local storage
    MkInit
  | -- | After the config is loaded from the local storage
    MkConfigLoaded (Maybe Config)
  deriving stock (Show, Eq)

frontendHead :: (DomBuilder t m) => m ()
frontendHead = do
  el "title" $ text "Diverk"
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "content" =: "width=device-width, initial-scale=1.0"
    )
    blank

  elAttr
    "link"
    ( "href" =: "css/styles.css"
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank
  elAttr
    "link"
    ( "href" =: "fontawesome/css/all.css"
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank

frontendBody ::
  forall t m.
  ( DomBuilder t m,
    Prerender t m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    Route.Set t m,
    Route.ToUrl m,
    Route.Ask t m
  ) =>
  m ()
frontendBody = do
  dyRoute <- Route.ask
  evSettingsLoaded <- fmap MkConfigLoaded <$> load

  rec dyState <- holdDyn MkInit $ leftmost [evSettingsLoaded, evSettingsSaved]
      let dyDarkModeOnRouteChange = getDarkMode <$> dyState <* dyRoute
          evDarkModeOnRouteChange = catMaybes $ updated dyDarkModeOnRouteChange
      void $ setDarkModeOn evDarkModeOnRouteChange
      evSettingsSaved <-
        switchHold never =<< dyn (route <$> dyRoute <*> dyState)

  pure ()
  where
    getConfig (MkConfigLoaded mbConfig) = mbConfig
    getConfig _ = Nothing
    getDarkMode = preview (to getConfig . _Just . darkMode)

route ::
  ( DomBuilder t m,
    Prerender t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    Route.Set t m,
    Route.ToUrl m,
    Route.Ask t m
  ) =>
  Route.Route ->
  State ->
  m (Event t State)
route Route.Settings (MkConfigLoaded mbConfig) = do
  evOk <- Settings.page mbConfig
  evSaved <- save evOk
  Route.set $ Route.Push (Route.Browse []) <$ evSaved
  pure $ MkConfigLoaded . Just <$> evSaved
route (Route.Browse path) (MkConfigLoaded (Just config)) = do
  Browse.page config path
  pure never
route (Route.Search keywords) (MkConfigLoaded (Just (MkConfig owner repo (Just token) _))) = do
  Search.page owner repo token keywords
  pure never
route Route.Home (MkConfigLoaded (Just _)) = do
  ev <- getPostBuild
  Route.set $ Route.Replace (Route.Browse []) <$ ev
  pure never
route _ (MkConfigLoaded Nothing) = do
  ev <- getPostBuild
  Route.set $ Route.Replace Route.Settings <$ ev
  pure never
route Route.About (MkConfigLoaded mbConfig) = do
  About.page hasToken
  pure never
  where
    hasToken = isJust $ coToken =<< mbConfig
route _ _ = pure never
