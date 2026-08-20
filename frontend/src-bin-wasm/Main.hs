{-# LANGUAGE ForeignFunctionInterface #-}

module WasmMain (main) where

import qualified Frontend
import GHC.Wasm.Prim
import qualified Language.Javascript.JSaddle.Wasm as JSaddle
import Reflex.Dom.Core
import qualified Route

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main _ =
  JSaddle.run $
    mainWidgetWithHead Frontend.head $
      Route.run Frontend.body
