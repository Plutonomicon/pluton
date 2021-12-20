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

import Data.Kind (Type)
import Data.Nat (Nat (S, Z), nat0, nat1, nat2)
import Data.Text (Text)
import Debug.Trace
import Ledger.Scripts (Validator (..))
import Plutarch
import Plutarch.Bool
import Plutarch.ByteString
import Plutarch.Integer
import Plutarch.String (pfromText)
import Plutarch.Unit
import PlutusCore qualified as PLC

plutarchValidator :: Validator
plutarchValidator =
  Validator $ compile validator

{- | TODO: Rewrite raw 'Untyped Plutarch' to use the types from typed eDSL
 Upstream the types to Plutarch
-}
validator :: ClosedTerm (PByteString :--> PByteString :--> ScriptContext :--> PUnit)
validator =
  plam $ \datum (_redeemer :: Term s PByteString) ctxT ->
    -- TODO: Monadic DSL for threading through match/let/if?
    pmatch' ctxT $ \(ctx :: ScriptContext s) ->
      pmatch' (scriptContextTxInfo ctx) $ \(txInfo :: TxInfo s) ->
        plet (txInfoSignatories txInfo) $ \signatories ->
          plet (hasElem £ punsafeCoerce datum £ signatories) $ \(isBeneficiary :: Term s PBool) ->
            pif isBeneficiary (pcon PUnit) $ pTrace "plu:not-beneficiary" perror

-- EXP

data ScriptContext s = ScriptContext
  { scriptContextTxInfo :: Term s TxInfo
  , _scriptContextPurpose :: Term s POpaque
  }

data TxInfo s = TxInfo
  { txInfoSignatories :: Term s (PList POpaque)
  }

-- We first write instances by hand, then generalize.
instance PlutusType ScriptContext where
  type PInner ScriptContext b = ScriptContext

  {-
  pcon' (ScriptContext a b) =
    (PLC.ConstrData #)
      -- Only sum type, indexed at 0
      £ (pcon' $ PPairData
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
    plet (pTrace "plu:pUnConstrData" $ PLC.UnConstrData #£ dat) $ \dat' ->
      pmatch' (pTrace "plu:dat'" dat') $ \(PPairData _n0 products :: PPairData PInteger (PList POpaque) s) ->
        plet (atIndex £ (0 :: Term s PInteger) £ products) $ \a ->
          -- TODO: Allow lazy retrieval of fields
          plet (atIndex £ (1 :: Term s PInteger) £ products) $ \b ->
            f (ScriptContext (punsafeCoerce a) b)

instance PlutusType TxInfo where
  type PInner TxInfo _ = TxInfo
  pcon' = undefined
  pmatch' dat f =
    plet (pTrace "plu:pUnConstrData" $ PLC.UnConstrData #£ dat) $ \dat' ->
      pmatch' (pTrace "plu:dat'" dat') $ \(PPairData _ products :: PPairData PInteger (PList POpaque) s) ->
        plet (atIndex £ (7 :: Term s PInteger) £ products) $ \x ->
          f (TxInfo $ PLC.UnListData #£ punsafeCoerce x)

-------------------------------
-- To upstream (after clean up)
-------------------------------
instance PEq POpaque where
  a £== b =
    pBuiltin PLC.EqualsData £ a £ b

-- Pair of Data

data PPairData a b s = PPairData (Term s a) (Term s b)

instance PlutusType (PPairData a b) where
  type PInner (PPairData a b) _ = PPairData a b
  pcon' (PPairData a b) = PLC.MkPairData #£ a £ b -- There is no MkPair
  pmatch' pair f =
    -- TODO: use delay/force to avoid evaluating `pair` twice?
    plet (PLC.FstPair #£ pair) $ \a ->
      plet (PLC.SndPair #£ pair) $ \b ->
        f $ PPairData a b

-- List

data PList a s
  = PNil
  | PCons (Term s a) (Term s (PList a))

instance PlutusType (PList a) where
  type PInner (PList a) _ = PList a
  pcon' PNil = undefined -- TODO??
  pcon' (PCons x xs) = PLC.MkCons #£ x £ xs
  pmatch' list f =
    plet (PLC.NullList #£ list) $ \isEmpty ->
      pif
        (punsafeCoerce isEmpty)
        (f PNil)
        $ plet
          (PLC.HeadList #£ list)
          ( \head ->
              plet (PLC.TailList #£ list) $ \tail ->
                f $ PCons head tail
          )

hasElem :: PEq a => ClosedTerm (a :--> PList a :--> PBool)
hasElem =
  pfix £$ plam $ \self k list ->
    pmatch' list $ \case
      PNil -> pTrace "plu:hasElem:error" $ pcon PFalse
      PCons x xs ->
        pif
          (k £== x)
          (pcon PTrue)
          (self £ k £ xs)

atIndex :: ClosedTerm (PInteger :--> PList a :--> a)
atIndex =
  pfix £$ plam $ \self n' list ->
    pmatch' (pTrace "plu:n" list) $ \case
      PNil -> pTrace "plu:atIndex:err" perror
      PCons x xs ->
        pif
          (n' £== 0)
          x
          (self £ (n' - 1) £ xs)

-- Trace

pTrace :: Text -> Term s a -> Term s a
pTrace s f = PLC.Trace #£ pfromText s £ f

-- Builtins

-- | Builtin function application
(#£) :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). PLC.DefaultFun -> Term s a -> Term s b
(#£) f af = pBuiltin f £ af

infixl 9 #£

pBuiltin :: PLC.DefaultFun -> Term s a
pBuiltin builtin =
  phoistAcyclic $ forceN (forceLevel builtin) $ punsafeBuiltin builtin
  where
    forceN :: Nat -> Term s a -> Term s a
    forceN Z = id
    forceN (S n) = pforce . punsafeCoerce . forceN n

class ForceLevel a where
  forceLevel :: a -> Nat

instance ForceLevel PLC.DefaultFun where
  forceLevel = \case
    PLC.EqualsData -> nat0
    PLC.ConstrData -> nat0
    PLC.UnConstrData -> nat0
    PLC.UnListData -> nat0
    PLC.IData -> nat0
    PLC.MkPairData -> nat0
    PLC.FstPair -> nat2
    PLC.SndPair -> nat2
    PLC.MkCons -> nat1
    PLC.NullList -> nat1
    PLC.HeadList -> nat1
    PLC.TailList -> nat1
    PLC.Trace -> nat1
    PLC.MkNilData -> nat0
    x -> traceShow x undefined -- TODO: Use TypeError
