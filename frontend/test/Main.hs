module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Tests.Route as Route

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "Tests" [Route.test]
