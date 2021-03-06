{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Example.Contract.Gift.Test qualified as GiftTest
import Example.Contract.Gift.Validator.Plutarch qualified as Gift
import Pluton qualified
import Plutus.Trace.Emulator qualified as Em

main :: IO ()
main = do
  Pluton.smoke
  -- Gift contract example
  putStrLn "\n== Sample contract - Gift: emulator == "
  Em.runEmulatorTraceIO $ GiftTest.smokeTrace Gift.plutarchValidator
  putStrLn "\n== Sample contract - Gift: tests == "
  GiftTest.tests
