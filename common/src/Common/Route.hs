{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common.Route
  ( BackendRoute,
    FrontendRoute (..),
    FinalRoute (..),
    fullRouteEncoder,
  )
where

import Common.Model (Branch (..), Hash (..), Owner (..), Repo (..))
import Control.Category ((.))
import Control.Monad.Except (MonadError)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Obelisk.Route
  ( Encoder,
    FullRoute (..),
    PageName,
    R,
    SegmentResult (..),
    mkFullRouteEncoder,
    pathComponentEncoder,
    pathParamEncoder,
    singlePathSegmentEncoder,
    unitEncoder,
    unwrappedEncoder,
    pattern (:/),
  )
import Obelisk.Route.TH (deriveRouteComponent)
import Prelude hiding (id, (.))

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  MkMissing :: BackendRoute ()

-- Frontend routes

data FinalRoute :: * -> * where
  MkTree :: FinalRoute Hash

data FrontendRoute :: * -> * where
  MkHome :: FrontendRoute ()
  MkRepo :: FrontendRoute (Owner, (Repo, (Branch, R FinalRoute)))

fullRouteEncoder ::
  Encoder
    (Either Text)
    Identity
    (R (FullRoute BackendRoute FrontendRoute))
    PageName
fullRouteEncoder =
  mkFullRouteEncoder
    (FullRoute_Backend MkMissing :/ ())
    ( \case
        MkMissing -> PathSegment "missing" $ unitEncoder mempty
    )
    ( \case
        MkHome -> PathEnd $ unitEncoder mempty
        Common.Route.MkRepo ->
          PathSegment "repo"
            . pathParamEncoder unwrappedEncoder
            . pathParamEncoder unwrappedEncoder
            . pathParamEncoder unwrappedEncoder
            $ finalRouteEncoder
    )

finalRouteEncoder ::
  (MonadError Text check, MonadError Text parse) =>
  Encoder check parse (R FinalRoute) PageName
finalRouteEncoder = pathComponentEncoder $ \case
  MkTree -> PathSegment "tree" $ singlePathSegmentEncoder . unwrappedEncoder

-- | This is the function that will be used to generate links to frontend routes.
concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute,
      ''FinalRoute
    ]
