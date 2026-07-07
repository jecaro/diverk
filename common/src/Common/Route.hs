{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Route
  ( BackendRoute,
    FrontendRoute (..),
    fullRouteEncoder,
  )
where

import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Obelisk.Route
  ( Encoder,
    FullRoute (..),
    PageName,
    R,
    SegmentResult (..),
    mkFullRouteEncoder,
    pathOnlyEncoder,
    unitEncoder,
    pattern (:/),
  )
import Obelisk.Route.TH (deriveRouteComponent)
import Prelude hiding (id, (.))

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  MkMissing :: BackendRoute ()

-- Frontend routes

data FrontendRoute :: * -> * where
  MkHome :: FrontendRoute ()
  MkSettings :: FrontendRoute ()
  MkBrowse :: FrontendRoute [Text]
  MkSearch :: FrontendRoute [Text]
  MkAbout :: FrontendRoute ()

-- Derive GShow/GEq/GCompare/UniverseSome/ArgDict for the route GADTs. This
-- splice must appear *before* fullRouteEncoder: a top-level TH splice starts a
-- new declaration group, so the instances it generates are only in scope for
-- code that follows it (GHC 9.x enforces this; GHC 8.10 was more lenient).
concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]

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
        MkBrowse -> PathSegment "repo" pathOnlyEncoder
        MkSettings -> PathSegment "settings" $ unitEncoder mempty
        MkSearch -> PathSegment "search" pathOnlyEncoder
        MkAbout -> PathSegment "about" $ unitEncoder mempty
    )
