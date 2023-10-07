{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Frontend (frontend) where

import About (about)
import Browse (browse)
import Common.Model (Config (..))
import Common.Route (FrontendRoute (..))
import Configuration (configuration)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (isJust)
import LocalStorage (load, save)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Generated.Static (static)
import Obelisk.Route (R, pattern (:/))
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute (..), askRoute)
import Reflex.Dom.Core
import Search (search)

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

  rec dyState <-
        holdDyn MkInit $ leftmost [evSettingsLoaded, evSettingsSaved]
      evSettingsSaved <-
        switchHold never =<< dyn (route <$> dyRoute <*> dyState)

  pure ()

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
route (MkConfiguration :/ ()) (MkConfigLoaded mbConfig) = do
  evOk <- configuration mbConfig
  evSaved <- save evOk
  setRoute $ MkBrowse :/ [] <$ evSaved
  pure $ MkConfigLoaded . Just <$> evSaved
route (MkBrowse :/ path) (MkConfigLoaded (Just config)) = do
  browse config path
  pure never
route
  (MkSearch :/ keywords)
  (MkConfigLoaded (Just (MkConfig owner repo (Just token)))) = do
    search owner repo token keywords
    pure never
route (MkHome :/ ()) (MkConfigLoaded (Just _)) = do
  setRoute . (MkBrowse :/ [] <$) =<< getPostBuild
  pure never
route _ (MkConfigLoaded Nothing) = do
  setRoute . (MkConfiguration :/ () <$) =<< getPostBuild
  pure never
route (MkAbout :/ ()) (MkConfigLoaded mbConfig) = do
  about hasToken
  pure never
  where
    hasToken = isJust $ coToken =<< mbConfig
route _ _ = pure never
