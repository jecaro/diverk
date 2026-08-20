{-# LANGUAGE QuasiQuotes #-}

module JSDOM.Storage.Extra (save, load, clear) where

import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.String.Interpolate as Interpolate
import qualified Data.Text as Text
import qualified JSDOM
import qualified JSDOM.Storage as JSDOM
import qualified JSDOM.Types as JSDOM
import qualified JSDOM.Window as JSDOM
import qualified Language.Javascript.JSaddle as JSaddle

getLocalStorageUnchecked :: JSaddle.JSM JSDOM.Storage
getLocalStorageUnchecked = JSDOM.currentWindowUnchecked >>= JSDOM.getLocalStorage

save :: (JSDOM.ToJSVal a) => Text.Text -> a -> JSaddle.JSM ()
save key val = do
  ls <- getLocalStorageUnchecked
  JSDOM.setItem ls key =<< JSaddle.valToJSON val

-- Parse a JSON string, returns null on any error
safeParseJSON :: JSaddle.JSString -> JSaddle.JSM JSaddle.JSVal
safeParseJSON json = JSaddle.call (JSaddle.eval script) JSaddle.global [json]
  where
    script :: Text.Text
    script =
      [Interpolate.iii|
      (function (str) {
        try {
          return JSON.parse(str);
        }
        catch (e) {
          return null;
        }
      }
      )|]

load :: (JSaddle.FromJSVal a) => Text.Text -> JSaddle.JSM (Maybe a)
load key =
  MaybeT.runMaybeT $ do
    jsString <- MaybeT.MaybeT $ flip JSDOM.getItem key =<< getLocalStorageUnchecked
    jsVal <- MaybeT.MaybeT $ toMaybe =<< safeParseJSON jsString
    MaybeT.MaybeT $ JSaddle.fromJSVal jsVal
  where
    toMaybe :: JSaddle.JSVal -> JSaddle.JSM (Maybe JSaddle.JSVal)
    toMaybe jsVal = do
      JSaddle.valIsNull jsVal >>= \case
        True -> pure Nothing
        False -> pure $ Just jsVal

clear :: Text.Text -> JSaddle.JSM ()
clear key = getLocalStorageUnchecked >>= flip JSDOM.removeItem key
