-- | Client-side routing for the Diverk SPA.
--
-- The design mirrors Obelisk's @Obelisk.Route.Frontend@: two typeclasses let
-- widgets express routing needs as constraints rather than explicit parameters.
--
-- * 'SetRoute' — a widget that wants to navigate calls 'setRoute'. Under the
--   hood this is 'EventWriterT': navigation events bubble up through the widget
--   tree and are collected at the top without any explicit plumbing.
--
-- * 'RouteToUrl' — a widget that needs to render a URL (e.g. for an @\<a href\>@)
--   calls 'askRouteToUrl'. Under the hood this is 'ReaderT': 'renderRoute' is
--   threaded down implicitly.
--
-- 'runRouteViewT' wires both transformers together, reads the initial URL,
-- listens for back/forward navigation, and drives the browser History API:
-- 'Push' adds a history entry, 'Replace' overwrites the current one (used for
-- the home-redirect to avoid a spurious back-button step).
module Route
  ( Route (..),
    Nav (..),
    parseRoute,
    renderRoute,
    SetRoute (..),
    RouteToUrl (..),
    AskRoute (..),
    routeLink,
    runRouteViewT,
  )
where

import Control.Lens ((%~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, runReaderT)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import JSDOM (currentWindowUnchecked)
import JSDOM.Generated.EventTarget (addEventListener)
import JSDOM.Generated.History (pushState, replaceState)
import JSDOM.Generated.Location (getPathname)
import JSDOM.Generated.Window (getHistory, getLocation)
import JSDOM.Types (EventListener (..))
import Language.Javascript.JSaddle (MonadJSM, function, liftJSM, toJSVal)
import Reflex.Dom.Core hiding (Home, Search)

data Route
  = Home
  | Browse [Text]
  | Settings
  | Search [Text]
  | About
  deriving stock (Eq, Show)

data Nav
  = Push Route
  | Replace Route
  deriving stock (Eq, Show)

parseRoute :: Text -> Route
parseRoute path = case filter (not . T.null) (T.splitOn "/" path) of
  [] -> Home
  ("repo" : rest) -> Browse rest
  ["settings"] -> Settings
  ("search" : rest) -> Search rest
  ["about"] -> About
  _ -> Home

renderRoute :: Route -> Text
renderRoute Home = "/"
renderRoute (Browse []) = "/repo"
renderRoute (Browse path) = "/repo/" <> T.intercalate "/" path
renderRoute Settings = "/settings"
renderRoute (Search kws) = "/search/" <> T.intercalate "/" kws
renderRoute About = "/about"

data RouteEnv t = RouteEnv
  { reDyRoute :: Dynamic t Route
  , reRenderRoute :: Route -> Text
  }

class (Reflex t, Monad m) => SetRoute t m | m -> t where
  setRoute :: Event t Nav -> m ()

class Monad m => RouteToUrl m where
  askRouteToUrl :: m (Route -> Text)

class (Reflex t, Monad m) => AskRoute t m | m -> t where
  askRoute :: m (Dynamic t Route)

instance (Reflex t, Monad m) => SetRoute t (EventWriterT t [Nav] m) where
  setRoute ev = tellEvent (pure <$> ev)

instance (Reflex t, Monad m) => RouteToUrl (ReaderT (RouteEnv t) m) where
  askRouteToUrl = asks reRenderRoute

instance RouteToUrl m => RouteToUrl (EventWriterT t w m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m) => AskRoute t (ReaderT (RouteEnv t) m) where
  askRoute = asks reDyRoute

instance (Reflex t, AskRoute t m) => AskRoute t (EventWriterT t w m) where
  askRoute = lift askRoute

routeLink ::
  forall t m a.
  (DomBuilder t m, SetRoute t m, RouteToUrl m) =>
  Route ->
  m a ->
  m a
routeLink route inner = do
  toUrl <- askRouteToUrl
  let cfg =
        (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & elementConfig_initialAttributes .~ ("href" =: toUrl route)
          & elementConfig_eventSpec
            %~ addEventSpecFlags
              (Proxy :: Proxy (DomBuilderSpace m))
              Click
              (const preventDefault)
  (aEl, result) <- element "a" cfg inner
  setRoute $ Push route <$ domEvent Click aEl
  pure result

runRouteViewT ::
  ( TriggerEvent t m,
    MonadHold t m,
    PerformEvent t m,
    MonadJSM m,
    MonadJSM (Performable m)
  ) =>
  EventWriterT t [Nav] (ReaderT (RouteEnv t) m) () ->
  m ()
runRouteViewT widget = do
  initialRoute <- liftJSM $ do
    win <- currentWindowUnchecked
    parseRoute <$> (getPathname =<< getLocation win)
  (evNavRoute, triggerNavRoute) <- newTriggerEvent
  (evPopRoute, triggerPopRoute) <- newTriggerEvent
  liftJSM $ do
    win <- currentWindowUnchecked
    cb <- function $ \_ _ _ -> do
      path <- getPathname =<< getLocation =<< currentWindowUnchecked
      liftIO $ triggerPopRoute (parseRoute path)
    cbVal <- toJSVal cb
    addEventListener win ("popstate" :: Text) (Just (EventListener cbVal)) False
  dyRoute <- holdDyn initialRoute $ leftmost [evPopRoute, evNavRoute]
  (_, evNavs) <- flip runReaderT (RouteEnv dyRoute renderRoute) $ runEventWriterT widget
  performEvent_ $ ffor evNavs $ \navs -> liftJSM $ do
    hist <- getHistory =<< currentWindowUnchecked
    mapM_ (\nav -> do
      let r = case nav of { Push r' -> r'; Replace r' -> r' }
      case nav of
        Push _    -> pushState hist (Nothing :: Maybe Text) ("" :: Text) (Just (renderRoute r))
        Replace _ -> replaceState hist (Nothing :: Maybe Text) ("" :: Text) (Just (renderRoute r))
      liftIO $ triggerNavRoute r) navs
