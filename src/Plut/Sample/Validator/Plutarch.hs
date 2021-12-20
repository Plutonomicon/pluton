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
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Type.Nat
import Ledger.Scripts (Validator (..))
import Plutarch
import Plutarch.Bool
import Plutarch.ByteString
import Plutarch.Integer
import Plutarch.String (PString, pfromText)
import Plutarch.Unit
import PlutusCore qualified as PLC

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
          plet (hasElem £ punsafeCoerce datum £ signatories) $ \(isBeneficiary :: Term s PBool) ->
            pif isBeneficiary (pcon PUnit) $ "plu:not-beneficiary" !£ perror

data ScriptContext s = ScriptContext
  { scriptContextTxInfo :: Term s TxInfo
  , -- TODO
    _scriptContextPurpose :: Term s POpaque
  }

data TxInfo s = TxInfo
  { txInfoSignatories :: Term s (PList POpaque)
  -- TODO: other fields
  }

-- NOTE: We first write instances by hand, then latter will generalize for all sum and product types.
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
    plet ("plu:pUnConstrData" !£ (UnConstrData #£ punsafeCoerce dat)) $ \dat' ->
      pmatch' ("plu:dat'" !£ dat') $ \(PPairData _n0 products :: PPairData PInteger (PList POpaque) s) ->
        plet (atIndex £ (0 :: Term s PInteger) £ products) $ \a ->
          -- TODO: Allow lazy retrieval of fields
          plet (atIndex £ (1 :: Term s PInteger) £ products) $ \b ->
            f (ScriptContext (punsafeCoerce a) b)

instance PlutusType TxInfo where
  type PInner TxInfo _ = TxInfo
  pcon' = undefined
  pmatch' dat f =
    plet ("plu:pUnConstrData" !£ UnConstrData #£ punsafeCoerce dat) $ \dat' ->
      pmatch' ("plu:dat'" !£ dat') $ \(PPairData _ products :: PPairData PInteger (PList POpaque) s) ->
        -- TODO: hardcoding index
        plet (atIndex £ (7 :: Term s PInteger) £ products) $ \x ->
          f (TxInfo $ UnListData #£ punsafeCoerce x)

-------------------------------
-- To upstream (after clean up)
-------------------------------
instance PEq POpaque where
  a £== b =
    EqualsData #£ a £ b

-- Pair of Data

data PPairData a b s = PPairData (Term s a) (Term s b)

instance PlutusType (PPairData a b) where
  type PInner (PPairData a b) _ = PPairData a b
  pcon' (PPairData a b) = MkPairData #£ a £ b -- There is no MkPair
  pmatch' pair f =
    -- TODO: use delay/force to avoid evaluating `pair` twice?
    plet (FstPair #£ pair) $ \a ->
      plet (SndPair #£ pair) $ \b ->
        f $ PPairData a b

-- List

-- TODO: Rename to PListData?
data PList a s
  = PNil
  | PCons (Term s a) (Term s (PList a))

instance PlutusType (PList a) where
  type PInner (PList a) _ = PList a
  pcon' PNil = undefined -- TODO
  pcon' (PCons x xs) = MkCons #£ x £ xs
  pmatch' list f =
    plet (NullList #£ list) $ \isEmpty ->
      pif
        (punsafeCoerce isEmpty)
        (f PNil)
        $ plet
          (HeadList #£ list)
          ( \head ->
              plet (TailList #£ list) $ \tail ->
                f $ PCons head tail
          )

hasElem :: PEq a => ClosedTerm (a :--> PList a :--> PBool)
hasElem =
  pfix £$ plam $ \self k list ->
    pmatch' list $ \case
      PNil ->
        "plu:hasElem:fail"
          !£ pcon PFalse
      PCons x xs ->
        pif
          (k £== x)
          (pcon PTrue)
          (self £ k £ xs)

atIndex :: ClosedTerm (PInteger :--> PList a :--> a)
atIndex =
  pfix £$ plam $ \self n' list ->
    pmatch' ("plu:n" !£ list) $ \case
      PNil ->
        "plu:atIndex:err"
          !£ perror
      PCons x xs ->
        pif
          (n' £== 0)
          x
          (self £ (n' - 1) £ xs)

-- Builtins

{- | Type spec for PLC's untyped builtin

 The `forces` value determines the repeated application of `FORCE` when
 evaluating the builtin function.

 Example: UnConstrData #£ someData
-}
data PBuiltin (forces :: Nat) (args :: [k -> Type]) (res :: k -> Type) where
  UnConstrData :: PBuiltin Nat0 '[POpaque] (PPairData PInteger (PList POpaque))
  UnListData :: PBuiltin Nat0 '[POpaque] (PList POpaque)
  MkPairData :: PBuiltin Nat0 '[a, b] (PPairData a b)
  FstPair :: PBuiltin Nat2 '[PPairData a b] a
  SndPair :: PBuiltin Nat2 '[PPairData a b] b
  MkCons :: PBuiltin Nat1 '[a, PList a] (PList a)
  NullList :: PBuiltin Nat1 '[a] PBool
  HeadList :: PBuiltin Nat1 '[PList a] a
  TailList :: PBuiltin Nat1 '[PList a] (PList a)
  EqualsData :: PBuiltin Nat0 '[POpaque, POpaque] PBool
  Trace :: PBuiltin Nat1 '[PString, a] a

type family PBuiltinType (args :: [k -> Type]) (res :: k -> Type) where
  PBuiltinType '[] res = res
  PBuiltinType (a ': as) res = a :--> PBuiltinType as res

pBuiltinTerm ::
  forall args res forces s.
  PBuiltin forces args res ->
  Term s (PBuiltinType args res)
pBuiltinTerm b =
  phoistAcyclic $ case b of
    UnConstrData ->
      force @forces Proxy . punsafeBuiltin $ PLC.UnConstrData
    UnListData ->
      force @forces Proxy . punsafeBuiltin $ PLC.UnListData
    MkPairData ->
      force @forces Proxy . punsafeBuiltin $ PLC.MkPairData
    FstPair ->
      force @forces Proxy . punsafeBuiltin $ PLC.FstPair
    SndPair ->
      force @forces Proxy . punsafeBuiltin $ PLC.SndPair
    MkCons ->
      force @forces Proxy . punsafeBuiltin $ PLC.MkCons
    NullList ->
      force @forces Proxy . punsafeBuiltin $ PLC.NullList
    HeadList ->
      force @forces Proxy . punsafeBuiltin $ PLC.HeadList
    TailList ->
      force @forces Proxy . punsafeBuiltin $ PLC.TailList
    EqualsData ->
      force @forces Proxy . punsafeBuiltin $ PLC.EqualsData
    Trace ->
      force @forces Proxy . punsafeBuiltin $ PLC.Trace
  where
    force :: forall forces s a. SNatI forces => Proxy (forces :: Nat) -> Term s a -> Term s a
    force Proxy =
      let sn = snat :: SNat forces
       in forceN (snatToNat sn)
    forceN :: forall s a. Nat -> Term s a -> Term s a
    forceN Z = id
    forceN (S n) = pforce . punsafeCoerce . forceN n

(#£) ::
  forall
    k
    (args :: [k -> Type])
    (res :: k -> Type)
    (a :: k -> Type)
    (b :: k -> Type)
    (forces :: Nat)
    (s :: k).
  (PBuiltinType args res ~ (a :--> b)) =>
  PBuiltin forces args res ->
  Term s a ->
  Term s b
(#£) b = (pBuiltinTerm b £)
infixl 9 #£

-- Handy builtin aliases

pTrace :: Text -> Term s a -> Term s a
pTrace s f = Trace #£ pfromText s £ f

(!£) :: forall k (s :: k) (a :: k -> Type). Text -> Term s a -> Term s a
(!£) = pTrace
infixl 8 !£
