-- | This is more or less a Prelude for writing Pluton eDSL.
module Pluton (smoke, module X) where

import Plutarch as X (ClosedTerm, PlutusType (pcon'), Term, pcon, pfix, phoistAcyclic, plam, plet, (#), (#$), type (:-->))
import Plutarch qualified
import Plutarch qualified as PLC
import Plutarch.Bool as X
import Plutarch.Integer as X
import Pluton.Run qualified as Run
import Pluton.Run.Evaluate qualified as Smoke
import Pluton.Types.Builtin as X
import Pluton.Types.Builtin.Data ()
import Pluton.Types.Builtin.List qualified as BL
import Pluton.Types.Builtin.List qualified as Smoke
import PlutusCore.Default qualified as PLC

fibs :: Term s (PInteger :--> BL.PList PInteger)
fibs = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      (BL.singleton 0)
      $ pif
        (n #== 1)
        (BL.mkList [1, 0])
        $ plet (self # (n - 1)) $ \a ->
          plet (self # (n - 2)) $ \b ->
            BL.cons
              (BL.head a + BL.head b)
              a

-- Useful during development when running bin/ghcid
smoke :: IO ()
smoke = do
  let eval :: ClosedTerm a -> IO ()
      eval p = do
        print $ Smoke.evalPlutarch p
        print $ Run.scriptSize $ Run.compile p
      fourtyTwo = 42 :: Term s PInteger
      intNil =
        Plutarch.punsafeConstant $
          PLC.Some $
            PLC.ValueOf (PLC.DefaultUniList PLC.DefaultUniInteger) []
      intSingle :: Term s (Smoke.PList PInteger)
      intSingle = pBuiltinFun @'PLC.MkCons @'[PInteger] # fourtyTwo # intNil
      intSingle' :: Term s (Smoke.PList PInteger)
      intSingle' = Plutarch.pforce (PLC.punsafeBuiltin PLC.MkCons) # fourtyTwo # intNil
  eval fourtyTwo
  eval $ "IData" !# (pBuiltinFun @'PLC.IData @'[] # fourtyTwo)
  eval $ "UnIData" !# (pBuiltinFun @'PLC.UnIData @'[] #$ pBuiltinFun @'PLC.IData @'[] # fourtyTwo)
  eval $ "nil" !# intNil
  eval $ "intSingle manual" !# intSingle'
  eval $ "intSingle" !# intSingle
  eval $ "mkcons" !# pcon' (Smoke.PCons (1 :: Term s PInteger) $ "nil" !# Smoke.nil)
  eval $ "list simple" !# Smoke.singleton (1 :: Term s PInteger)
  eval $ "fibs" !# (fibs # 5)
  eval $ "making list" !# Smoke.mkList [1 :: Term s PInteger, 2, 3]
