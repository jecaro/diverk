module JSDOM.Storage.Extra (save, load, clear) where

import Control.Lens ((^.))
import Control.Monad ((<=<))
import Data.Text (Text)
import JSDOM (currentWindowUnchecked)
import JSDOM.Storage (getItem, removeItem, setItem)
import JSDOM.Types (Storage, ToJSVal (..))
import JSDOM.Window (getLocalStorage)
import Language.Javascript.JSaddle (FromJSVal (..), JSM, JSString, JSVal, js1, jsg, valToJSON)

getLocalStorageUnchecked :: JSM Storage
getLocalStorageUnchecked = currentWindowUnchecked >>= getLocalStorage

save :: ToJSVal a => Text -> a -> JSM ()
save key val = do
  ls <- getLocalStorageUnchecked
  setItem ls key =<< valToJSON val

load :: FromJSVal a => Text -> JSM (Maybe a)
load key =
  orNothing (fromJSVal <=< jsonParse)
    =<< flip getItem key
    =<< getLocalStorageUnchecked
  where
    jsonParse :: JSString -> JSM JSVal
    jsonParse string = jsg ("JSON" :: Text) ^. js1 ("parse" :: Text) string
    orNothing = maybe (pure Nothing)

clear :: Text -> JSM ()
clear key = getLocalStorageUnchecked >>= flip removeItem key
