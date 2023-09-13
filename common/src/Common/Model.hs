{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Model (Owner (..), Repo (..), contentsURL, usersURL) where

import Control.Lens (makeWrapped, (^.), _Wrapped)
import Data.Text (Text)
import qualified Data.Text as T

newtype Owner = MkOwner {unOwner :: Text}
  deriving stock (Eq, Show, Read)

newtype Repo = MkRepo {unRepo :: Text}
  deriving stock (Eq, Show, Read)

concat
  <$> mapM
    makeWrapped
    [ ''Owner,
      ''Repo
    ]

githubBaseURL :: Text
githubBaseURL = "https://api.github.com"

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
