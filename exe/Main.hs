{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Pluton qualified as Pluton
import Example.Contract.Gift.Test qualified as SampleTest
import Example.Contract.Gift.Validator.Plutarch qualified as Sample
import Plutus.Trace.Emulator qualified as Em

main :: IO ()
main = do
  Pluton.smoke
  Em.runEmulatorTraceIO $ SampleTest.smokeTrace Sample.plutarchValidator
  SampleTest.tests
