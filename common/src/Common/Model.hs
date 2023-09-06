{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Model
  ( Owner (..),
    Branch (..),
    Repo (..),
    Hash (..),
    repoUrl,
    treeUrl,
  )
where

import Control.Lens (makeWrapped, (^.), _Wrapped)
import Data.Text (Text)
import qualified Data.Text as T

newtype Owner = MkOwner {unOwner :: Text}
  deriving (Eq, Show, Read)

newtype Repo = MkRepo {unRepo :: Text}
  deriving (Eq, Show, Read)

newtype Branch = MkBranch {unBranch :: Text}
  deriving (Eq, Show, Read)

newtype Hash = MkHash {unHash :: Text}
  deriving (Eq, Show, Read)

makeWrapped ''Branch
makeWrapped ''Hash
makeWrapped ''Owner
makeWrapped ''Repo

githubUrl :: Owner -> Repo -> [Text] -> Text
githubUrl owner repo path =
  T.intercalate "/" $
    [ "https://api.github.com/repos",
      owner ^. _Wrapped,
      repo ^. _Wrapped
    ]
      <> path

repoUrl :: Owner -> Repo -> Branch -> Text
repoUrl owner repo branch = githubUrl owner repo ["branches", branch ^. _Wrapped]

treeUrl :: Owner -> Repo -> Hash -> Text
treeUrl owner repo hash = githubUrl owner repo ["git", "trees", hash ^. _Wrapped]
