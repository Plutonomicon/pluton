module Pluton.Types.Builtin.ListSpec (tests) where

-- import Hedgehog
-- import Pluton
-- import Pluton.Evaluate qualified as Eval
-- import Pluton.Types.Builtin.List
import Test.Tasty

-- import Test.Tasty.Hedgehog

tests :: TestTree
tests =
  testGroup
    "polymorphic"
    []

{- testProperty "constructs polymorphic list within PLC universe" . property $ do
  let list1 = mkList [1 :: Term s PInteger, 2, 3]
  Eval.evalPlutarch list1 === Eval.evalPlutarch list1
-}
