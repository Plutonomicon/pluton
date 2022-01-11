-- | This is more or less a Prelude for writing Pluton eDSL.
module Pluton (smoke, module X) where

import Plutarch as X (ClosedTerm, PlutusType (pcon'), Term, pcon, pfix, phoistAcyclic, plam, plet, (#), (#$), type (:-->))
import Plutarch.Bool as X
import Plutarch.Integer as X
import Pluton.Run qualified as Run
import Pluton.Run.Evaluate qualified as Smoke

-- Useful during development when running bin/ghcid
smoke :: IO ()
smoke = do
  let eval :: ClosedTerm a -> IO ()
      eval p = do
        print $ Smoke.evalPlutarch p
        print $ Run.scriptBudget $ Run.compile p
      fourtyTwo = 42 :: Term s PInteger
  eval fourtyTwo
