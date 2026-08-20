{-# LANGUAGE RecursiveDo #-}

module Frontend (head, body) where

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Control.Monad.Fix as MonadFix
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Maybe as Maybe
import qualified LocalStorage
import qualified Model
import qualified Page.About as About
import qualified Page.Browse as Browse
import qualified Page.Search as Search
import qualified Page.Settings as Settings
import Reflex.Dom.Core hiding (Home, Search)
import qualified Route
import qualified Theme
import qualified Witherable
import Prelude hiding (head)

data State
  = -- | The initial state: before the config is loaded from the local storage
    MkInit
  | -- | After the config is loaded from the local storage
    MkConfigLoaded (Maybe Model.Config)
  deriving stock (Show, Eq)

head :: (DomBuilder t m) => m ()
head = do
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

body ::
  forall t m.
  ( DomBuilder t m,
    Prerender t m,
    MonadFix.MonadFix m,
    MonadHold t m,
    PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO.MonadIO (Performable m),
    Route.Set t m,
    Route.Ask t m
  ) =>
  m ()
body = do
  dyRoute <- Route.ask
  evSettingsLoaded <- fmap MkConfigLoaded <$> LocalStorage.load

  rec dyState <- holdDyn MkInit $ leftmost [evSettingsLoaded, evSettingsSaved]
      let dyDarkModeOnRouteChange = getDarkMode <$> dyState <* dyRoute
          evDarkModeOnRouteChange =
            Witherable.catMaybes $
              updated dyDarkModeOnRouteChange
      Monad.void $ Theme.setDarkModeOn evDarkModeOnRouteChange
      evSettingsSaved <-
        switchHold never =<< dyn (route <$> dyRoute <*> dyState)

  pure ()
  where
    getConfig (MkConfigLoaded mbConfig) = mbConfig
    getConfig _ = Nothing
    getDarkMode = Lens.preview (Lens.to getConfig . Lens._Just . Model.darkMode)

route ::
  ( DomBuilder t m,
    Prerender t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix.MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO.MonadIO (Performable m),
    Route.Set t m,
    Route.Ask t m
  ) =>
  Route.Route ->
  State ->
  m (Event t State)
route Route.Settings (MkConfigLoaded mbConfig) = do
  evOk <- Settings.page mbConfig
  evSaved <- LocalStorage.save evOk
  Route.set $ Route.Push (Route.Browse []) <$ evSaved
  pure $ MkConfigLoaded . Just <$> evSaved
route (Route.Browse path) (MkConfigLoaded (Just config)) = do
  Browse.page config path
  pure never
route
  (Route.Search keywords)
  (MkConfigLoaded (Just (Model.MkConfig owner repo (Just token) _))) = do
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
    hasToken = Maybe.isJust $ Model.coToken =<< mbConfig
route _ _ = pure never
