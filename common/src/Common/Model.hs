{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Model (Owner (..), Repo (..)) where

import Control.Lens (makeWrapped)
import Data.Text (Text)

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
