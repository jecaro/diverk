{-# LANGUAGE ForeignFunctionInterface #-}

module WasmMain (main) where

import Frontend (frontendBody, frontendHead)
import GHC.Wasm.Prim
import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm
import Reflex.Dom.Core
import Route (runRouteViewT)

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main _ =
  JSaddle.Wasm.run $
    mainWidgetWithHead frontendHead $
      runRouteViewT frontendBody
