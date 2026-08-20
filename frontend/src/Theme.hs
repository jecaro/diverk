module Theme (setDarkModeOn, getSystemDarkModeEvent) where

import qualified Data.Text as Text
import qualified JSDOM
import qualified JSDOM.Generated.Document as JSDOM
import qualified JSDOM.Generated.Element as JSDOM
import qualified JSDOM.Generated.MediaQueryList as JSDOM
import qualified JSDOM.Generated.Window as JSDOM
import qualified Language.Javascript.JSaddle as JSaddle
import Reflex.Dom.Core
import Reflex.Extra (onClient)

setDarkMode :: Bool -> JSaddle.JSM ()
setDarkMode dark = do
  documentElement <- JSDOM.getDocumentElementUnchecked =<< JSDOM.currentDocumentUnchecked
  JSDOM.setAttribute documentElement ("data-theme" :: Text.Text) theme
  where
    theme :: Text.Text
    theme
      | dark = "dark"
      | otherwise = "light"

setDarkModeOn ::
  forall m t.
  ( Prerender t m,
    Applicative m
  ) =>
  Event t Bool ->
  m (Event t ())
setDarkModeOn = onClient . performEvent . fmap (JSaddle.liftJSM . setDarkMode)

getSystemDarkMode :: JSaddle.JSM Bool
getSystemDarkMode =
  JSDOM.currentWindowUnchecked
    >>= flip JSDOM.matchMedia query
    >>= JSDOM.getMatches
  where
    query :: Text.Text
    query = "(prefers-color-scheme: dark)"

getSystemDarkModeEvent ::
  forall m t. (Prerender t m, MonadHold t m) => m (Event t Bool)
getSystemDarkModeEvent = do
  dyDarkMode <- prerender (pure False) . JSaddle.liftJSM $ getSystemDarkMode
  -- Return only the first event, we're only interested in the initial value
  headE $ updated dyDarkMode
