{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Example.Contract.Gift.Validator.Plutarch (plutarchValidator) where

import Ledger qualified
import Ledger.Scripts (Validator (..))
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.ByteString (PByteString)
import Plutarch.DataRepr
import Plutarch.Lift
import Plutarch.List
import Plutarch.ScriptContext
import Plutarch.Trace
import Plutarch.Unit (PUnit (..))
import PlutusCore qualified as PLC
import PlutusTx (Data)

plutarchValidator :: Validator
plutarchValidator =
  Validator $ compile validator

-- TODO: remove these instances after https://github.com/Plutonomicon/plutarch/pull/66
deriving via
  (PIsDataReprInstances PTxInfo Ledger.TxInfo)
  instance
    (PMatch PTxInfo)

deriving via
  (PIsDataReprInstances PTxInfo Ledger.TxInfo)
  instance
    PIsData PTxInfo

deriving via
  (PIsDataReprInstances PTxInfo Ledger.TxInfo)
  instance
    PLift PTxInfo

deriving via
  (PBuiltinType POpaque Data)
  instance
    PLift POpaque

instance PIsData (PBuiltinList POpaque) where
  pfromData x = punsafeCoerce $ pasList # pforgetData x
  pdata x = punsafeBuiltin PLC.ListData # x

instance PIsData POpaque where
  pfromData = punsafeCoerce
  pdata = punsafeCoerce

instance PEq POpaque where
  a #== b = punsafeBuiltin PLC.EqualsData # a # b

instance PIsDataRepr PTxInfo where
  type
    PIsDataReprRepr PTxInfo =
      '[ '[ PBuiltinList PTxInInfo,
            PBuiltinList PTxOut,
            PValue,
            PValue,
            PBuiltinList POpaque,
            POpaque,
            POpaque,
            PBuiltinList POpaque,
            POpaque,
            POpaque
          ]
       ]
  pmatchRepr dat f =
    (pmatchDataRepr dat) ((DRHCons (f . PTxInfo)) $ DRHNil)

-- | TODO: Gradually rewrite raw 'Untyped Plutarch' to use the types from typed
-- eDSL, and then upstream the types to Plutarch
validator :: ClosedTerm (PByteString :--> PByteString :--> PScriptContext :--> PUnit)
validator =
  plam $ \datum (_redeemer :: Term s PByteString) ctxT ->
    -- TODO: Monadic DSL for threading through match/let/if?
    pmatch ctxT $ \(PScriptContext fields) ->
      let txInfo' = pfromData $ pdhead # fields
       in pmatch txInfo' $ \(PTxInfo fields2) ->
            let signatories' = pdhead #$ pdtail #$ pdtail #$ pdtail #$ pdtail #$ pdtail #$ pdtail #$ pdtail # fields2
                signatories = pfromData signatories'
             in plet (pelem # punsafeCoerce datum # signatories) $ \(isBeneficiary :: Term s PBool) ->
                  pif isBeneficiary (pcon PUnit) $ ptrace "plu:not-beneficiary" perror
