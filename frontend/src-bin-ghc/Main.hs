module Main (main) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as Text
import qualified Frontend
import qualified Language.Javascript.JSaddle.WebSockets as JSaddle
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.WebSockets as WebSocket
import Reflex.Dom.Core
import qualified Route

githubProxy :: HTTP.Manager -> [Text.Text] -> Wai.Application
githubProxy mgr pathSegments request respond = do
  let path = Text.intercalate "/" pathSegments
      queryString = BC.unpack $ Wai.rawQueryString request
      url = "https://api.github.com/" <> Text.unpack path <> queryString
      headers =
        filter
          ((`elem` allowedHeaders) . fst)
          (Wai.requestHeaders request)
  initRequest <- HTTP.parseRequest url
  let githubRequest =
        initRequest
          { HTTP.requestHeaders = headers,
            HTTP.decompress = const False
          }
  response <- HTTP.httpLbs githubRequest mgr
  respond $
    Wai.responseLBS
      (HTTP.responseStatus response)
      (HTTP.responseHeaders response)
      (HTTP.responseBody response)
  where
    allowedHeaders = ["Authorization", "Accept", "Content-Type", "User-Agent"]

main :: IO ()
main = do
  manager <- HTTP.newTlsManager
  app <-
    JSaddle.jsaddleOr
      WebSocket.defaultConnectionOptions
      (mainWidgetWithHead Frontend.head $ Route.run Frontend.body)
      $ fallback manager
  putStrLn "serving app on http://localhost:3000"
  Warp.run 3000 app
  where
    static :: Wai.Application
    static = Wai.staticApp $ Wai.defaultFileServerSettings "static/out"

    fallback ::
      HTTP.Manager ->
      Wai.Request ->
      (Wai.Response -> IO Wai.ResponseReceived) ->
      IO Wai.ResponseReceived
    fallback manager request respond = case Wai.pathInfo request of
      ("css" : _) -> static request respond
      ("fontawesome" : _) -> static request respond
      ("api" : "github" : rest) -> githubProxy manager rest request respond
      _ -> JSaddle.jsaddleApp request respond
