module Request (contentsRequest, usersRequest) where

import Common.Model (Owner, Repo, Token)
import Control.Lens ((<>~), (^.), _Wrapped)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

usersRequest :: Maybe Token -> Owner -> XhrRequest ()
usersRequest mbToken owner =
  xhrRequest
    "GET"
    (usersURL owner)
    (requestConfig mbToken)

contentsRequest :: Maybe Token -> Owner -> Repo -> [Text] -> XhrRequest ()
contentsRequest mbToken owner repo path =
  xhrRequest
    "GET"
    (contentsURL owner repo path)
    (requestConfig mbToken)

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

githubBaseURL :: Text
githubBaseURL = "https://api.github.com"
