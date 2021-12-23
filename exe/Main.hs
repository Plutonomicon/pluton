{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Pluton qualified as Pluton
import Pluton.Sample.Test qualified as SampleTest
import Pluton.Sample.Validator.Plutarch qualified as Sample
import Plutus.Trace.Emulator qualified as Em

main :: IO ()
main = do
  Pluton.smoke
  Em.runEmulatorTraceIO $ SampleTest.smokeTrace Sample.plutarchValidator
  SampleTest.tests
