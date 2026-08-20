module Tests.Route (test) where

import qualified Data.Text as Text
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Gen as Hedgehog
import qualified Hedgehog.Range as Hedgehog
import qualified Route
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Tasty

test :: Tasty.TestTree
test =
  Tasty.testGroup
    "Route"
    [ Tasty.testPropertyNamed
        "parse . render == id"
        "prop_parse_render_roundtrip"
        $ Hedgehog.property
        $ do
          route <- Hedgehog.forAll genRoute
          Route.parse (Route.render route) === route
    ]

genRoute :: Hedgehog.Gen Route.Route
genRoute =
  Hedgehog.choice
    [ pure Route.Home,
      Route.Browse <$> genSegments,
      pure Route.Settings,
      Route.Search <$> genSegments,
      pure Route.About
    ]

genSegments :: Hedgehog.Gen [Text.Text]
genSegments =
  Hedgehog.list (Hedgehog.linear 0 5) $
    Hedgehog.text (Hedgehog.linear 1 20) Hedgehog.alphaNum
