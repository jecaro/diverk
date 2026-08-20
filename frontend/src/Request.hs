module Request
  ( contents,
    rateLimit,
    search,
    users,
  )
where

import Control.Lens ((<>~), (^.))
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Model
import qualified Network.HTTP.Types.URI as HTTP
import Reflex.Dom.Core

users :: Maybe Model.Token -> Model.Owner -> XhrRequest ()
users mbToken owner =
  xhrRequest "GET" (usersURL owner) (requestConfig mbToken)

contents ::
  Maybe Model.Token -> Model.Owner -> Model.Repo -> [Text.Text] -> XhrRequest ()
contents mbToken owner repo path =
  xhrRequest "GET" (contentsURL owner repo path) (requestConfig mbToken)

rateLimit :: Model.Token -> XhrRequest ()
rateLimit token =
  xhrRequest "GET" rateLimitURL (requestConfig $ Just token)

search ::
  Model.Token -> Model.Owner -> Model.Repo -> [Text.Text] -> XhrRequest ()
search token owner repo keywords =
  xhrRequest "GET" (searchURL <> queryParams) (requestConfig $ Just token)
  where
    queryParams =
      Text.decodeUtf8 $
        HTTP.renderSimpleQuery
          True
          [ ( "q",
              Text.encodeUtf8
                . Text.unwords
                $ keywords
                  <> [ "repo:"
                         <> owner ^. Lens._Wrapped
                         <> "/"
                         <> repo ^. Lens._Wrapped
                     ]
            ),
            -- That is the maximum the GibHub API allows
            ("per_page", "100")
          ]

requestConfig :: Maybe Model.Token -> XhrRequestConfig ()
requestConfig mbToken = def & xhrRequestConfig_headers <>~ tokenHeader mbToken

tokenHeader :: Maybe Model.Token -> Map.Map Text.Text Text.Text
tokenHeader (Just token) =
  "Authorization" =: ("Bearer " <> token ^. Lens._Wrapped)
tokenHeader Nothing = mempty

contentsURL :: Model.Owner -> Model.Repo -> [Text.Text] -> Text.Text
contentsURL owner repo path =
  Text.intercalate "/" $
    [ githubBaseURL,
      "repos",
      owner ^. Lens._Wrapped,
      repo ^. Lens._Wrapped,
      "contents"
    ]
      <> path

usersURL :: Model.Owner -> Text.Text
usersURL owner =
  Text.intercalate
    "/"
    [githubBaseURL, "users", owner ^. Lens._Wrapped]

rateLimitURL :: Text.Text
rateLimitURL = githubBaseURL <> "/rate_limit"

searchURL :: Text.Text
searchURL = githubBaseURL <> "/search/code"

githubBaseURL :: Text.Text
githubBaseURL = "/api/github"
