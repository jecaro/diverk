module JSDOM.Storage.Extra (save, load, clear) where

import Data.Text (Text)
import JSDOM (currentWindowUnchecked)
import JSDOM.Storage (getItem, removeItem, setItem)
import JSDOM.Types (FromJSString, Storage, ToJSString)
import JSDOM.Window (getLocalStorage)
import Language.Javascript.JSaddle (JSM)

getLocalStorageUnchecked :: JSM Storage
getLocalStorageUnchecked = currentWindowUnchecked >>= getLocalStorage

save :: ToJSString a => Text -> a -> JSM ()
save key val = getLocalStorageUnchecked >>= \ls -> setItem ls key val

load :: FromJSString a => Text -> JSM (Maybe a)
load key = getLocalStorageUnchecked >>= flip getItem key

clear :: Text -> JSM ()
clear key = getLocalStorageUnchecked >>= flip removeItem key
