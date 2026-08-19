module Main (main) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Frontend (frontendBody, frontendHead)
import Language.Javascript.JSaddle.WebSockets (jsaddleApp, jsaddleOr)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai (Application, pathInfo, rawQueryString, requestHeaders, responseLBS)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom.Core
import qualified Route

githubProxy :: HC.Manager -> [T.Text] -> Application
githubProxy mgr pathSegments req respond = do
  let path = T.intercalate "/" pathSegments
      qs = BC.unpack (rawQueryString req)
      url = "https://api.github.com/" <> T.unpack path <> qs
      hdrs = filter ((`elem` ["Authorization", "Accept", "Content-Type", "User-Agent"]) . fst) (requestHeaders req)
  initReq <- HC.parseRequest url
  let ghReq = initReq {HC.requestHeaders = hdrs}
  resp <- HC.httpLbs ghReq mgr
  -- http-client decompresses gzip but leaves Content-Encoding in the headers;
  -- strip it so the browser doesn't try to decode already-plain bytes.
  let respHeaders =
        filter
          ((`notElem` ["Content-Encoding", "Transfer-Encoding"]) . fst)
          (HC.responseHeaders resp)
  respond $
    responseLBS
      (HC.responseStatus resp)
      respHeaders
      (HC.responseBody resp)

main :: IO ()
main = do
  mgr <- newTlsManager
  let static = staticApp (defaultFileServerSettings "static/out")
      fallback req respond = case pathInfo req of
        ("css" : _) -> static req respond
        ("fontawesome" : _) -> static req respond
        ("api" : "github" : rest) -> githubProxy mgr rest req respond
        _ -> jsaddleApp req respond
  app <-
    jsaddleOr
      defaultConnectionOptions
      (mainWidgetWithHead frontendHead $ Route.run frontendBody)
      fallback
  putStrLn "serving app on http://localhost:3000"
  run 3000 app
