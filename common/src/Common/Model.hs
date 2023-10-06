{-# LANGUAGE TemplateHaskell #-}

module Common.Model
  ( Owner (..),
    Repo (..),
    Token (..),
    Config (..),
    Path (..),
    owner,
    repo,
    token,
  )
where

import Control.Lens (abbreviatedFields, makeLensesWith, makeWrapped)
import Data.Text (Text)

newtype Owner = MkOwner {unOwner :: Text}
  deriving stock (Eq, Show, Read)

newtype Repo = MkRepo {unRepo :: Text}
  deriving stock (Eq, Show, Read)

newtype Token = MkToken {unToken :: Text}
  deriving stock (Eq, Show, Read)

data Config = MkConfig
  { coOwner :: Owner,
    coRepo :: Repo,
    coToken :: Maybe Token
  }
  deriving stock (Eq, Show, Read)

newtype Path = MkPath
  { unPath :: [Text]
  }
  deriving stock (Eq, Show)

concat
  <$> mapM
    makeWrapped
    [ ''Owner,
      ''Path,
      ''Repo,
      ''Token
    ]

makeLensesWith abbreviatedFields ''Config
