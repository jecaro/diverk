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
  MkConfiguration :: FrontendRoute ()
  MkBrowse :: FrontendRoute [Text]
  MkSearch :: FrontendRoute [Text]
  MkAbout :: FrontendRoute ()

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
        MkConfiguration -> PathSegment "config" $ unitEncoder mempty
        MkSearch -> PathSegment "search" pathOnlyEncoder
        MkAbout -> PathSegment "about" $ unitEncoder mempty
    )

-- | This is the function that will be used to generate links to frontend routes.
concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]
