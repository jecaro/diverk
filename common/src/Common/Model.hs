{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Common.Model
  ( Owner (..),
    Branch (..),
    Repo (..),
    GitTree (..),
    Hash (..),
    owner,
    repo,
    branch,
    repoUrl,
    treeUrl,
  )
where

import Control.Lens
  ( abbreviatedFields,
    makeLensesWith,
    makeWrapped,
    (^.),
    _Wrapped,
  )
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

data GitTree = MkGitTree
  { reOwner :: !Owner,
    reRepo :: !Repo,
    reBranch :: !Branch
  }
  deriving (Eq, Show)

makeLensesWith abbreviatedFields ''GitTree

makeWrapped ''Branch
makeWrapped ''Hash
makeWrapped ''Owner
makeWrapped ''Repo

githubUrl :: GitTree -> [Text] -> Text
githubUrl tree path =
  T.intercalate "/" $
    [ "https://api.github.com/repos",
      tree ^. owner . _Wrapped,
      tree ^. repo . _Wrapped
    ]
      <> path

repoUrl :: GitTree -> Text
repoUrl tree = githubUrl tree ["branches", tree ^. branch . _Wrapped]

treeUrl :: GitTree -> Hash -> Text
treeUrl tree hash = githubUrl tree ["git", "trees", hash ^. _Wrapped]
