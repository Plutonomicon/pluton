{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
validator :: forall s. Term s (PByteString :--> PByteString :--> PByteString :--> PUnit)
validator =
  plam $ \datum (_redeemer :: Term s PByteString) (ctx :: Term s PByteString) ->
    plet
      (pTrace "plu:atProduct" $ atProduct 0 $ pTrace "plu:ctx" ctx)
      $ \txInfo ->
        plet (atProduct 7 $ pTrace "plu:txInfo" txInfo) $ \signatories' ->
          plet ((PLC.UnListData #) £ punsafeCoerce (pTrace "plu:sig" signatories')) $ \signatories ->
            plet (pTrace "plu:hasElem" $ hasElem (punsafeCoerce datum) signatories) $ \(isBeneficiary :: Term s PBool) ->
              pif isBeneficiary (pcon PUnit) $ pTrace "plu:not-beneficiary" perror

instance PEq POpaque where
  a £== b =
    pBuiltin PLC.EqualsData £ a £ b

-- List functions

hasElem :: forall s. Term s POpaque -> Term s POpaque -> Term s PBool
hasElem k list' =
  go £ list'
  where
    -- TODO: why can't i use phoistAcyclic  here?
    go :: Term s (POpaque :--> PBool)
    go =
      pfix £$ plam $ \self list ->
        pmatch' list $ \case
          PNil -> pTrace "plu:hasElem:error" $ pcon PFalse
          PCons x xs ->
            pif
              (k £== x)
              (pcon PTrue)
              (self £ xs)

atProduct :: Term s PInteger -> Term s a -> Term s POpaque
atProduct n dat =
  plet (pTrace "plu:pUnConstrData" $ (PLC.UnConstrData #) £ punsafeCoerce dat) $ \dat' ->
    pmatch' (pTrace "plu:dat'" dat') $ \(PPair _ products :: PPair s) ->
      atIndex n products

atIndex :: Term s PInteger -> Term s POpaque -> Term s POpaque
atIndex n =
  (go £ n £)
  where
    go :: Term s (PInteger :--> POpaque :--> POpaque)
    go = phoistAcyclic $
      pfix
        £$ plam
        $ \self n' list ->
          pmatch' (pTrace "plu:n" list) $ \case
            PNil -> pTrace "plu:atIndex:err" perror
            PCons x xs ->
              pif
                (n' £== 0)
                x
                (self £ (n' - 1) £ xs)

-- All of these are working with `Data`

data PPair s = PPair (Term s POpaque) (Term s POpaque)

instance PlutusType PPair where
  type PInner PPair _ = POpaque
  pcon' (PPair a b) = (PLC.MkPairData #) £ a £ b -- There is no MkPair, plutus build constructs pair of data
  pmatch' pair f =
    -- TODO: use delay/force to avoid evaluating `pair` twice?
    plet ((PLC.FstPair #) £ pair) $ \a ->
      plet ((PLC.SndPair #) £ pair) $ \b ->
        f $ PPair a b

data PList s
  = PNil
  | PCons (Term s POpaque) (Term s POpaque)

instance PlutusType PList where
  type PInner PList _ = POpaque
  pcon' PNil = undefined -- TODO??
  pcon' (PCons x xs) = (PLC.MkCons #) £ x £ xs
  pmatch' list f =
    plet ((PLC.NullList #) £ list) $ \isEmpty ->
      pif
        (punsafeCoerce isEmpty)
        (f PNil)
        $ plet
          ((PLC.HeadList #) £ list)
          ( \head ->
              plet ((PLC.TailList #) £ list) $ \tail ->
                f $ PCons head tail
          )

-- Trace

pTrace :: forall a s. Text -> Term s a -> Term s a
pTrace s f = pBuiltin PLC.Trace £ pfromText s £ f

-- Builtins

(#) :: forall k (s :: k) (a :: k -> Type). PLC.DefaultFun -> Term s a
(#) = pBuiltin
infixl 6 #

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
    PLC.UnConstrData -> nat0
    PLC.UnListData -> nat0
    PLC.MkPairData -> nat0
    PLC.FstPair -> nat2
    PLC.SndPair -> nat2
    PLC.MkCons -> nat1
    PLC.NullList -> nat1
    PLC.HeadList -> nat1
    PLC.TailList -> nat1
    PLC.Trace -> nat1
    _ -> undefined
