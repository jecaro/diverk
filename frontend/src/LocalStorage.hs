module LocalStorage (save, load, clear) where

import Data.Text (Text)
import JSDOM (currentWindowUnchecked)
import JSDOM.Generated.Storage (getItem, removeItem, setItem)
import JSDOM.Generated.Window (getLocalStorage)
import JSDOM.Types (FromJSString, Storage, ToJSString)
import Language.Javascript.JSaddle (JSM)

getLocalStorage' :: JSM Storage
getLocalStorage' = currentWindowUnchecked >>= getLocalStorage

save :: ToJSString a => Text -> a -> JSM ()
save key val = getLocalStorage' >>= \ls -> setItem ls key val

load :: FromJSString a => Text -> JSM (Maybe a)
load key = getLocalStorage' >>= flip getItem key

clear :: Text -> JSM ()
clear key = getLocalStorage' >>= flip removeItem key
