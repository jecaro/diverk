{-# LANGUAGE TemplateHaskell #-}

module Common.Model
  ( Owner (..),
    Repo (..),
    Token (..),
    Config (..),
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

concat
  <$> mapM
    makeWrapped
    [ ''Owner,
      ''Repo,
      ''Token
    ]

makeLensesWith abbreviatedFields ''Config
