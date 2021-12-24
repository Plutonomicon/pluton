{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Example.Contract.Gift.Test qualified as GiftTest
import Example.Contract.Gift.Validator.Haskell qualified as Gift
import Example.Contract.Gift.Validator.Plutarch qualified as Gift
import Example.Contract.Gift.Validator.Pluto qualified as Gift
import Pluton qualified
import Pluton.Run qualified as Run
import Plutus.Trace.Emulator qualified as Em

main :: IO ()
main = do
  Pluton.smoke
  -- Gift contract example
  putStrLn "\n== Sample contract - Gift: sizes (haskell; pluto; plutarch) == "
  print $ Run.validatorBudget Gift.haskellValidator
  print $ Run.validatorBudget Gift.plutoValidator
  print $ Run.validatorBudget Gift.plutarchValidator
  putStrLn "\n== Sample contract - Gift: emulator == "
  Em.runEmulatorTraceIO $ GiftTest.smokeTrace Gift.plutarchValidator
  putStrLn "\n== Sample contract - Gift: tests == "
  GiftTest.tests
