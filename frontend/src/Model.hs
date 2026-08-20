{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Model
  ( Owner (..),
    Repo (..),
    Token (..),
    Config (..),
    Path (..),
    darkMode,
    owner,
    repo,
    token,
  )
where

import qualified Control.Lens as Lens
import qualified Data.Text as Text

newtype Owner = MkOwner {unOwner :: Text.Text}
  deriving stock (Eq, Show, Read)

newtype Repo = MkRepo {unRepo :: Text.Text}
  deriving stock (Eq, Show, Read)

newtype Token = MkToken {unToken :: Text.Text}
  deriving stock (Eq, Show, Read)

data Config = MkConfig
  { coOwner :: Owner,
    coRepo :: Repo,
    coToken :: Maybe Token,
    coDarkMode :: Bool
  }
  deriving stock (Eq, Show, Read)

newtype Path = MkPath
  { unPath :: [Text.Text]
  }
  deriving stock (Eq, Show)

concat
  <$> mapM
    Lens.makeWrapped
    [ ''Owner,
      ''Path,
      ''Repo,
      ''Token
    ]

Lens.makeLensesWith Lens.abbreviatedFields ''Config
