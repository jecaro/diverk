{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Model (Owner (..), Repo (..), githubURL) where

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

githubURL :: Owner -> Repo -> [Text] -> Text
githubURL owner repo path =
  T.intercalate "/" $
    [ "https://api.github.com/repos",
      owner ^. _Wrapped,
      repo ^. _Wrapped,
      "contents"
    ]
      <> path
