-- | This is more or less a Prelude for writing Pluton eDSL.
module Pluton (smoke, module X) where

import Plutarch as X (Term)
import Plutarch.Integer as X
import Pluton.Evaluate qualified as Smoke

-- import Pluton.Types.Builtin ((!#))
-- import Pluton.Types.Builtin.List qualified as Smoke

smoke :: IO ()
smoke = do
  print $ Smoke.evalPlutarch (42 :: Term s PInteger)

-- print $ Smoke.evalPlutarch $ "making list" !# Smoke.mkList [1 :: Term s PInteger, 2, 3]
