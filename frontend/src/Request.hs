module Request
  ( contents,
    rateLimit,
    search,
    users,
  )
where

import Common.Model (Owner, Repo, Token)
import Control.Lens ((<>~), (^.), _Wrapped)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.URI (renderSimpleQuery)
import Reflex.Dom.Core

users :: Maybe Token -> Owner -> XhrRequest ()
users mbToken owner =
  xhrRequest "GET" (usersURL owner) (requestConfig mbToken)

contents :: Maybe Token -> Owner -> Repo -> [Text] -> XhrRequest ()
contents mbToken owner repo path =
  xhrRequest "GET" (contentsURL owner repo path) (requestConfig mbToken)

rateLimit :: Token -> XhrRequest ()
rateLimit token =
  xhrRequest "GET" rateLimitURL (requestConfig $ Just token)

search :: Token -> Owner -> Repo -> [Text] -> XhrRequest ()
search token owner repo keywords =
  xhrRequest "GET" (searchURL <> queryParams) (requestConfig $ Just token)
  where
    queryParams =
      decodeUtf8 $
        renderSimpleQuery
          True
          [ ( "q",
              encodeUtf8
                . T.unwords
                $ keywords
                  <> ["repo:" <> owner ^. _Wrapped <> "/" <> repo ^. _Wrapped]
            ),
            -- That is the maximum the GibHub API allows
            ("per_page", "100")
          ]

requestConfig :: Maybe Token -> XhrRequestConfig ()
requestConfig mbToken = def & xhrRequestConfig_headers <>~ tokenHeader mbToken

tokenHeader :: Maybe Token -> Map Text Text
tokenHeader (Just token) = "Authorization" =: ("Bearer " <> token ^. _Wrapped)
tokenHeader Nothing = mempty

contentsURL :: Owner -> Repo -> [Text] -> Text
contentsURL owner repo path =
  T.intercalate "/" $
    [ githubBaseURL,
      "repos",
      owner ^. _Wrapped,
      repo ^. _Wrapped,
      "contents"
    ]
      <> path

usersURL :: Owner -> Text
usersURL owner =
  T.intercalate
    "/"
    [ githubBaseURL,
      "users",
      owner ^. _Wrapped
    ]

rateLimitURL :: Text
rateLimitURL = githubBaseURL <> "/rate_limit"

searchURL :: Text
searchURL = githubBaseURL <> "/search/code"

githubBaseURL :: Text
githubBaseURL = "https://api.github.com"
