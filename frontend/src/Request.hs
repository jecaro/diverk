module Request (contentsRequest, usersRequest) where

import Common.Model (Owner, Repo)
import Control.Lens ((^.), _Wrapped)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

usersRequest :: Owner -> XhrRequest ()
usersRequest owner = xhrRequest "GET" (usersURL owner) def

contentsRequest :: Owner -> Repo -> [Text] -> XhrRequest ()
contentsRequest owner repo path =
  xhrRequest
    "GET"
    (contentsURL owner repo path)
    def

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
