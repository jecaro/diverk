-- | Client-side routing for the Diverk SPA.
--
-- Two typeclasses let widgets express routing needs as constraints rather than
-- explicit parameters.
--
-- * 'Set' — a widget that wants to navigate calls 'set'. Under the
--   hood this is 'EventWriterT': navigation events bubble up through the widget
--   tree and are collected at the top without any explicit plumbing.
--
-- * 'Ask' — a widget that needs the current route calls 'ask'. Under the
--   hood this is 'ReaderT': the dynamic route is threaded down implicitly.
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
    Ask (..),
    link,
    run,
  )
where

import Control.Lens ((%~))
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MonadIO
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Reader as ReaderT
import qualified Data.Foldable as Foldable
import qualified Data.Proxy as Proxy
import qualified Data.Text as Text
import qualified JSDOM
import qualified JSDOM.EventM as JSDOM
import qualified JSDOM.Generated.History as JSDOM
import qualified JSDOM.Generated.Location as JSDOM
import qualified JSDOM.Generated.Window as JSDOM
import qualified JSDOM.Generated.WindowEventHandlers as JSDOM
import qualified Language.Javascript.JSaddle as JSaddle
import Reflex.Dom.Core hiding (Home, Search, link)

data Route
  = Home
  | Browse [Text.Text]
  | Settings
  | Search [Text.Text]
  | About
  deriving stock (Eq, Show)

data Nav
  = Push Route
  | Replace Route
  deriving stock (Eq, Show)

get :: Nav -> Route
get (Push r) = r
get (Replace r) = r

parse :: Text.Text -> Route
parse path = case filter (not . Text.null) (Text.splitOn "/" path) of
  [] -> Home
  ("repo" : rest) -> Browse rest
  ["settings"] -> Settings
  ("search" : rest) -> Search rest
  ["about"] -> About
  _ -> Home

render :: Route -> Text.Text
render Home = "/"
render (Browse []) = "/repo"
render (Browse path) = "/repo/" <> Text.intercalate "/" path
render Settings = "/settings"
render (Search kws) = "/search/" <> Text.intercalate "/" kws
render About = "/about"

class (Reflex t, Monad m) => Set t m | m -> t where
  set :: Event t Nav -> m ()

class (Reflex t, Monad m) => Ask t m | m -> t where
  ask :: m (Dynamic t Route)

instance (Reflex t, Monad m) => Set t (EventWriterT t [Nav] m) where
  set event = tellEvent (pure <$> event)

instance (Reflex t, Monad m) => Ask t (ReaderT.ReaderT (Dynamic t Route) m) where
  ask = ReaderT.ask

instance (Reflex t, Ask t m) => Ask t (EventWriterT t w m) where
  ask = Trans.lift ask

link ::
  forall t m a.
  (DomBuilder t m, Set t m) =>
  Nav ->
  m a ->
  m a
link nav inner = do
  let route = get nav
      config =
        (def :: ElementConfig EventResult t (DomBuilderSpace m))
          & elementConfig_initialAttributes
          .~ ("href" =: render route)
          & elementConfig_eventSpec
          %~ addEventSpecFlags
            (Proxy.Proxy :: Proxy.Proxy (DomBuilderSpace m))
            Click
            (const preventDefault)
  (aElement, result) <- element "a" config inner
  set $ nav <$ domEvent Click aElement
  pure result

run ::
  ( TriggerEvent t m,
    MonadHold t m,
    PerformEvent t m,
    JSaddle.MonadJSM m,
    JSaddle.MonadJSM (Performable m)
  ) =>
  EventWriterT t [Nav] (ReaderT.ReaderT (Dynamic t Route) m) () ->
  m ()
run widget = do
  initialRoute <- JSaddle.liftJSM $ do
    win <- JSDOM.currentWindowUnchecked
    parse <$> (JSDOM.getPathname =<< JSDOM.getLocation win)

  (evPopRoute, triggerPopRoute) <- newTriggerEvent
  JSaddle.liftJSM $ do
    window <- JSDOM.currentWindowUnchecked
    Monad.void $ JSDOM.on window JSDOM.popState $ do
      path <-
        JSDOM.getPathname
          =<< JSDOM.getLocation
          =<< JSDOM.currentWindowUnchecked
      MonadIO.liftIO $ triggerPopRoute $ parse path

  (evNavRoute, triggerNavRoute) <- newTriggerEvent
  dyRoute <- holdDyn initialRoute $ leftmost [evPopRoute, evNavRoute]

  (_, evNavs) <- flip ReaderT.runReaderT dyRoute $ runEventWriterT widget
  performEvent_ $ ffor evNavs $ \navs -> JSaddle.liftJSM $ do
    history <- JSDOM.getHistory =<< JSDOM.currentWindowUnchecked
    Foldable.traverse_
      ( \nav -> do
          let pushOrReplace = case nav of
                Push _ -> JSDOM.pushState
                Replace _ -> JSDOM.replaceState
              route = get nav
          pushOrReplace
            history
            (Nothing :: Maybe Text.Text)
            ("" :: Text.Text)
            (Just (render route))
          MonadIO.liftIO $ triggerNavRoute route
      )
      navs
