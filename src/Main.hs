{-# LANGUAGE NoOverloadedStrings #-}

module Main (main) where

import Pluton.Sample.Test qualified as SampleTest
import Pluton.Sample.Validator.Plutarch qualified as Sample
import Plutus.Trace.Emulator qualified as Em

main :: IO ()
main = do
  Em.runEmulatorTraceIO $ SampleTest.smokeTrace Sample.plutarchValidator
  SampleTest.tests
