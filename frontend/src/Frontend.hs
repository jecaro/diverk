{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend (frontend) where

import Common.Model (Config (..), darkMode)
import Common.Route (FrontendRoute (..))
import Control.Lens (preview, to, _Just)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import LocalStorage (load, save)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute (..), askRoute)
import qualified Page.About as About
import qualified Page.Browse as Browse
import qualified Page.Search as Search
import qualified Page.Settings as Settings
import Reflex.Dom.Core
import Theme (setDarkModeOn)
import Witherable (catMaybes)

data State
  = -- | The initial state: before the config is loaded from the local storage
    MkInit
  | -- | After the config is loaded from the local storage
    MkConfigLoaded (Maybe Config)
  deriving stock (Show, Eq)

frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = frontendHead,
      _frontend_body = frontendBody
    }

frontendHead :: DomBuilder t m => m ()
frontendHead = do
  el "title" $ text "Diverk"
  elAttr
    "meta"
    ( "name" =: "viewport"
        <> "contents" =: "width=device-width, initial-scale=1.0"
    )
    blank

  elAttr
    "link"
    ( "href" =: $(static "css/styles.css")
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank
  elAttr
    "link"
    ( "href" =: $(static "fontawesome/css/all.css")
        <> "type" =: "text/css"
        <> "rel" =: "stylesheet"
    )
    blank

frontendBody ::
  forall t m.
  ( DomBuilder t m,
    Prerender t m,
    Routed t (R FrontendRoute) m,
    MonadFix m,
    MonadHold t m,
    PostBuild t m,
    SetRoute t (R FrontendRoute) m,
    RouteToUrl (R FrontendRoute) m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m)
  ) =>
  m ()
frontendBody = do
  evSettingsLoaded <- fmap MkConfigLoaded <$> load
  dyRoute <- askRoute

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
    SetRoute t (R FrontendRoute) m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    RouteToUrl (R FrontendRoute) m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    Routed t (R FrontendRoute) m
  ) =>
  R FrontendRoute ->
  State ->
  m (Event t State)
route (MkSettings :/ ()) (MkConfigLoaded mbConfig) = do
  evOk <- Settings.page mbConfig
  evSaved <- save evOk
  setRoute $ MkBrowse :/ [] <$ evSaved
  pure $ MkConfigLoaded . Just <$> evSaved
route (MkBrowse :/ path) (MkConfigLoaded (Just config)) = do
  Browse.page config path
  pure never
route
  (MkSearch :/ keywords)
  (MkConfigLoaded (Just (MkConfig owner repo (Just token) _))) = do
    Search.page owner repo token keywords
    pure never
route (MkHome :/ ()) (MkConfigLoaded (Just _)) = do
  setRoute . (MkBrowse :/ [] <$) =<< getPostBuild
  pure never
route _ (MkConfigLoaded Nothing) = do
  setRoute . (MkSettings :/ () <$) =<< getPostBuild
  pure never
route (MkAbout :/ ()) (MkConfigLoaded mbConfig) = do
  About.page hasToken
  pure never
  where
    hasToken = isJust $ coToken =<< mbConfig
route _ _ = pure never
