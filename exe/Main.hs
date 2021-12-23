{-# LANGUAGE NoOverloadedStrings #-}

module Main (main) where

import Pluton (PInteger, Term)
import Pluton.Evaluate qualified as Eval
import Pluton.Sample.Test qualified as SampleTest
import Pluton.Sample.Validator.Plutarch qualified as Sample
import Plutus.Trace.Emulator qualified as Em

main :: IO ()
main = do
  print $ Eval.evalPlutarch (fromInteger 42 :: Term s PInteger)
  Em.runEmulatorTraceIO $ SampleTest.smokeTrace Sample.plutarchValidator
  SampleTest.tests
