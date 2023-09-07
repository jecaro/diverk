{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Route
  ( BackendRoute,
    FrontendRoute (..),
    fullRouteEncoder,
  )
where

import Common.Model (Owner (..), Repo)
import Control.Category ((.))
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
    pathParamEncoder,
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

data FrontendRoute :: * -> * where
  MkHome :: FrontendRoute ()
  MkOwnerAndRepo :: FrontendRoute (Owner, (Repo, [Text]))

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
        MkOwnerAndRepo ->
          PathSegment "repo"
            . pathParamEncoder unwrappedEncoder
            . pathParamEncoder unwrappedEncoder
            $ pathOnlyEncoder
    )

-- | This is the function that will be used to generate links to frontend routes.
concat
  <$> mapM
    deriveRouteComponent
    [ ''BackendRoute,
      ''FrontendRoute
    ]
