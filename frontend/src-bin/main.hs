{-# LANGUAGE CPP #-}

#if defined(wasm32_HOST_ARCH)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif

module MyMain (main) where

import Frontend (frontendBody, frontendHead)
import Reflex.Dom.Core
import Route (runRouteViewT)

#if defined(wasm32_HOST_ARCH)
import GHC.Wasm.Prim
import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main _ =
  JSaddle.Wasm.run $
    mainWidgetWithHead frontendHead $
      runRouteViewT frontendBody
#else
import Language.Javascript.JSaddle.WebSockets (jsaddleApp, jsaddleOr)
import Network.Wai (pathInfo)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (defaultConnectionOptions)

main :: IO ()
main = do
  let static = staticApp (defaultFileServerSettings "static/out")
      fallback req respond = case pathInfo req of
        ("css" : _)         -> static req respond
        ("fontawesome" : _) -> static req respond
        _                   -> jsaddleApp req respond
  app <-
    jsaddleOr defaultConnectionOptions
      (mainWidgetWithHead frontendHead $ runRouteViewT frontendBody)
      fallback
  run 3000 app
#endif
