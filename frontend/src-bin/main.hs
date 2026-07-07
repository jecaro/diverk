{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}

module MyMain (main) where

import Common.Route (FrontendRoute, fullRouteEncoder)
import Control.Category ((.))
import Data.Functor.Identity (Identity (..))
import qualified Data.Text as T
import Frontend (frontendBody, frontendHead)
import GHC.Wasm.Prim
import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm
import Obelisk.Route
  ( Encoder,
    PageName,
    R,
    checkEncoder,
    hoistParse,
    reviewEncoder,
    rPrism,
    _FullRoute_Frontend,
    _ObeliskRoute_App,
  )
import Obelisk.Route.Frontend (runRouteViewT)
import Reflex.Dom.Core
import Prelude hiding ((.))

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main _ =
  JSaddle.Wasm.run $
    mainWidgetWithHead frontendHead $ do
      switchover <- getPostBuild
      runRouteViewT frontendEncoder switchover False frontendBody

-- Project the full route encoder down to a checked, frontend-only encoder,
-- exactly as Obelisk.Frontend.runFrontend does internally.
frontendEncoder :: Encoder Identity Identity (R FrontendRoute) PageName
frontendEncoder =
  validFullEncoder
    . hoistParse errorLeft (reviewEncoder (rPrism (_FullRoute_Frontend . _ObeliskRoute_App)))
  where
    validFullEncoder = either (error . T.unpack) id (checkEncoder fullRouteEncoder)
    errorLeft = \case
      Left _ -> error "main: unexpected non-app route reached the frontend"
      Right x -> Identity x
