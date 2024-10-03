{-# LANGUAGE QuasiQuotes #-}

module JSDOM.Storage.Extra (save, load, clear) where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.String.Interpolate (iii)
import Data.Text (Text)
import JSDOM (currentWindowUnchecked)
import JSDOM.Storage (getItem, removeItem, setItem)
import JSDOM.Types (Storage, ToJSVal (..))
import JSDOM.Window (getLocalStorage)
import Language.Javascript.JSaddle
  ( FromJSVal (..),
    JSM,
    JSString,
    JSVal,
    call,
    eval,
    global,
    valIsNull,
    valToJSON,
  )

getLocalStorageUnchecked :: JSM Storage
getLocalStorageUnchecked = currentWindowUnchecked >>= getLocalStorage

save :: ToJSVal a => Text -> a -> JSM ()
save key val = do
  ls <- getLocalStorageUnchecked
  setItem ls key =<< valToJSON val

-- Parse a JSON string, returns null on any error
safeParseJSON :: JSString -> JSM JSVal
safeParseJSON json = call (eval script) global [json]
  where
    script :: Text
    script =
      [iii|
      (function (str) {
        try {
          return JSON.parse(str);
        }
        catch (e) {
          return null;
        }
      }
      )|]

load :: FromJSVal a => Text -> JSM (Maybe a)
load key =
  runMaybeT $ do
    jsString <- MaybeT $ flip getItem key =<< getLocalStorageUnchecked
    jsVal <- MaybeT $ toMaybe =<< safeParseJSON jsString
    MaybeT $ fromJSVal jsVal
  where
    toMaybe :: JSVal -> JSM (Maybe JSVal)
    toMaybe jsVal = do
      valIsNull jsVal >>= \case
        True -> pure Nothing
        False -> pure $ Just jsVal

clear :: Text -> JSM ()
clear key = getLocalStorageUnchecked >>= flip removeItem key
