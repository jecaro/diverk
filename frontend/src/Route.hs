-- | Client-side routing for the Diverk SPA.
--
-- The design mirrors Obelisk's @Obelisk.Route.Frontend@: two typeclasses let
-- widgets express routing needs as constraints rather than explicit parameters.
--
-- * 'Set' — a widget that wants to navigate calls 'set'. Under the
--   hood this is 'EventWriterT': navigation events bubble up through the widget
--   tree and are collected at the top without any explicit plumbing.
--
-- * 'ToUrl' — a widget that needs to render a URL (e.g. for an @\<a href\>@)
--   calls 'toUrl'. Under the hood this is 'ReaderT': 'render' is
--   threaded down implicitly.
--
-- 'run' wires both transformers together, reads the initial URL,
-- listens for back/forward navigation, and drives the browser History API:
-- 'Push' adds a history entry, 'Replace' overwrites the current one (used for
-- the home-redirect to avoid a spurious back-button step).
module Route
  ( Route (..),
    Nav (..),
    get,
    parse,
    render,
    Set (..),
    ToUrl (..),
    Ask (..),
    link,
    run,
  )
where

import Control.Lens ((%~))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
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
import Reflex.Dom.Core hiding (Home, Search, link)

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

get :: Nav -> Route
get (Push r) = r
get (Replace r) = r

parse :: Text -> Route
parse path = case filter (not . T.null) (T.splitOn "/" path) of
  [] -> Home
  ("repo" : rest) -> Browse rest
  ["settings"] -> Settings
  ("search" : rest) -> Search rest
  ["about"] -> About
  _ -> Home

render :: Route -> Text
render Home = "/"
render (Browse []) = "/repo"
render (Browse path) = "/repo/" <> T.intercalate "/" path
render Settings = "/settings"
render (Search kws) = "/search/" <> T.intercalate "/" kws
render About = "/about"

data RouteEnv t = RouteEnv
  { reDyRoute :: Dynamic t Route,
    reRenderRoute :: Route -> Text
  }

class (Reflex t, Monad m) => Set t m | m -> t where
  set :: Event t Nav -> m ()

class (Monad m) => ToUrl m where
  toUrl :: m (Route -> Text)

class (Reflex t, Monad m) => Ask t m | m -> t where
  ask :: m (Dynamic t Route)

instance (Reflex t, Monad m) => Set t (EventWriterT t [Nav] m) where
  set ev = tellEvent (pure <$> ev)

instance (Reflex t, Monad m) => ToUrl (ReaderT (RouteEnv t) m) where
  toUrl = asks reRenderRoute

instance (ToUrl m) => ToUrl (EventWriterT t w m) where
  toUrl = lift toUrl

instance (Reflex t, Monad m) => Ask t (ReaderT (RouteEnv t) m) where
  ask = asks reDyRoute

instance (Reflex t, Ask t m) => Ask t (EventWriterT t w m) where
  ask = lift ask

link ::
  forall t m a.
  (DomBuilder t m, Set t m, ToUrl m) =>
  Nav ->
  m a ->
  m a
link nav inner = do
  renderFn <- toUrl
  let route = get nav
      cfg =
        (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & elementConfig_initialAttributes
          .~ ("href" =: renderFn route)
          & elementConfig_eventSpec
          %~ addEventSpecFlags
            (Proxy :: Proxy (DomBuilderSpace m))
            Click
            (const preventDefault)
  (aEl, result) <- element "a" cfg inner
  set $ nav <$ domEvent Click aEl
  pure result

run ::
  ( TriggerEvent t m,
    MonadHold t m,
    PerformEvent t m,
    MonadJSM m,
    MonadJSM (Performable m)
  ) =>
  EventWriterT t [Nav] (ReaderT (RouteEnv t) m) () ->
  m ()
run widget = do
  initialRoute <- liftJSM $ do
    win <- currentWindowUnchecked
    parse <$> (getPathname =<< getLocation win)
  (evNavRoute, triggerNavRoute) <- newTriggerEvent
  (evPopRoute, triggerPopRoute) <- newTriggerEvent
  liftJSM $ do
    win <- currentWindowUnchecked
    cb <- function $ \_ _ _ -> do
      path <- getPathname =<< getLocation =<< currentWindowUnchecked
      liftIO $ triggerPopRoute (parse path)
    cbVal <- toJSVal cb
    addEventListener win ("popstate" :: Text) (Just (EventListener cbVal)) False
  dyRoute <- holdDyn initialRoute $ leftmost [evPopRoute, evNavRoute]
  (_, evNavs) <- flip runReaderT (RouteEnv dyRoute render) $ runEventWriterT widget
  performEvent_ $ ffor evNavs $ \navs -> liftJSM $ do
    hist <- getHistory =<< currentWindowUnchecked
    mapM_
      ( \nav -> do
          let route = get nav
              pushOrReplace = case nav of
                Push _ -> pushState
                Replace _ -> replaceState
          pushOrReplace hist (Nothing :: Maybe Text) ("" :: Text) (Just (render route))
          liftIO $ triggerNavRoute route
      )
      navs
