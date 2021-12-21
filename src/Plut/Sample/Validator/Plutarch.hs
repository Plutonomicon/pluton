{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plut.Sample.Validator.Plutarch (plutarchValidator) where

import Ledger.Scripts (Validator (..))
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin (
  PList,
  PPair (..),
  (!£),
 )
import Plutarch.Builtin.Data ()
import Plutarch.Builtin.Data.Type
import Plutarch.Builtin.List qualified as BL
import Plutarch.Builtin.Pair qualified as BP
import Plutarch.ByteString
import Plutarch.Integer
import Plutarch.Unit

plutarchValidator :: Validator
plutarchValidator =
  Validator $ compile validator

{- | TODO: Gradually rewrite raw 'Untyped Plutarch' to use the types from typed
 eDSL, and then upstream the types to Plutarch
-}
validator :: ClosedTerm (PByteString :--> PByteString :--> ScriptContext :--> PUnit)
validator =
  plam $ \datum (_redeemer :: Term s PByteString) ctxT ->
    -- TODO: Monadic DSL for threading through match/let/if?
    pmatch' ctxT $ \(ctx :: ScriptContext s) ->
      pmatch' (scriptContextTxInfo ctx) $ \(txInfo :: TxInfo s) ->
        plet (txInfoSignatories txInfo) $ \signatories ->
          plet (BL.contains £ signatories £ punsafeCoerce datum) $ \(isBeneficiary :: Term s PBool) ->
            pif isBeneficiary (pcon PUnit) $ "plu:not-beneficiary" !£ perror

data ScriptContext s = ScriptContext
  { scriptContextTxInfo :: Term s TxInfo
  , -- TODO
    _scriptContextPurpose :: Term s PData
  }

data TxInfo s = TxInfo
  { txInfoSignatories :: Term s (PList PData)
  -- TODO: other fields
  }

-- NOTE: We first write instances by hand, then latter will generalize for all sum and product types.
instance PlutusType ScriptContext where
  type PInner ScriptContext b = ScriptContext

  {-
  pcon' (ScriptContext a b) =
    (PLC.ConstrData #)
      -- Only sum type, indexed at 0
      £ (pcon' $ PPair
            (punsafeCoerce $ (PLC.IData #) £ (0 :: Term s PInteger))
            (punsafeCoerce $ (PLC.MkNilData #) £ pcon PUnit))
      -- Fields of the first sum choice
      £ (pcon' $ PCons
          a
          (pcon' $ PCons
            b
            (pcon' PNil) ))
  -}
  pcon' = undefined
  pmatch' dat f =
    pmatch (punsafeCoerce dat) $ \case
      PDataConstr pair ->
        BP.matchPair pair $ \(PPair _n0 products :: PPair PInteger (PList PData) s) ->
          plet (BL.atIndex £ (0 :: Term s PInteger) £ products) $ \a ->
            -- TODO: Allow lazy retrieval of fields
            plet (BL.atIndex £ (1 :: Term s PInteger) £ products) $ \b ->
              f (ScriptContext (punsafeCoerce a) b)
      _ ->
        perror

instance PlutusType TxInfo where
  type PInner TxInfo _ = TxInfo
  pcon' = undefined
  pmatch' dat f =
    pmatch (punsafeCoerce dat) $ \case
      PDataConstr pair ->
        BP.matchPair pair $ \(PPair _n0 products :: PPair PInteger (PList PData) s) ->
          -- TODO: hardcoding index
          plet (BL.atIndex £ (7 :: Term s PInteger) £ products) $ \x ->
            pmatch' x $ \case
              PDataList xs ->
                f (TxInfo xs)
              _ ->
                perror
      _ ->
        perror
