module Main (main, limit, tests) where

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

-- Number of successful tests for each Hedgehog property.
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 1000)

tests :: TestTree
tests =
  localOption limit $
    testGroup
      "dsl"
      []
