module Theme (setDarkModeOn, getSystemDarkModeEvent) where

import Data.Text (Text)
import JSDOM (currentDocumentUnchecked, currentWindowUnchecked)
import JSDOM.Generated.Document (getDocumentElementUnchecked)
import JSDOM.Generated.Element (setAttribute)
import JSDOM.Generated.MediaQueryList (getMatches)
import JSDOM.Generated.Window (matchMedia)
import Language.Javascript.JSaddle (JSM, liftJSM)
import Reflex.Dom.Core
import Reflex.Extra (onClient)

setDarkMode :: Bool -> JSM ()
setDarkMode dark = do
  documentElement <- getDocumentElementUnchecked =<< currentDocumentUnchecked
  setAttribute documentElement ("data-theme" :: Text) theme
  where
    theme :: Text
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
setDarkModeOn = onClient . performEvent . fmap (liftJSM . setDarkMode)

getSystemDarkMode :: JSM Bool
getSystemDarkMode = currentWindowUnchecked >>= flip matchMedia query >>= getMatches
  where
    query :: Text
    query = "(prefers-color-scheme: dark)"

getSystemDarkModeEvent ::
  forall m t. (Prerender t m, Monad m, MonadHold t m) => m (Event t Bool)
getSystemDarkModeEvent = do
  dyDarkMode <- prerender (pure False) . liftJSM $ getSystemDarkMode
  -- Return only the first event, we're only interested in the initial value
  headE $ updated dyDarkMode
