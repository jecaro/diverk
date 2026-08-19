module Main (main) where

import Frontend (frontendBody, frontendHead)
import Language.Javascript.JSaddle.WebSockets (jsaddleApp, jsaddleOr)
import Network.Wai (pathInfo)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom.Core
import Route (runRouteViewT)

main :: IO ()
main = do
  let static = staticApp (defaultFileServerSettings "static/out")
      fallback req respond = case pathInfo req of
        ("css" : _) -> static req respond
        ("fontawesome" : _) -> static req respond
        _ -> jsaddleApp req respond
  app <-
    jsaddleOr
      defaultConnectionOptions
      (mainWidgetWithHead frontendHead $ runRouteViewT frontendBody)
      fallback
  putStrLn "serving app on http://localhost:3000"
  run 3000 app
