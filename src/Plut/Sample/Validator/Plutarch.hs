{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plut.Sample.Validator.Plutarch (plutarchValidator) where

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

-- | TODO: Rewrite raw 'Untyped Plutarch' to use the types from typed eDSL
validator :: forall s. Term s (PByteString :--> PByteString :--> PByteString :--> PUnit)
validator =
  plam $ \datum (_redeemer :: Term s PByteString) (ctx :: Term s PByteString) ->
    plet
      (pTrace "yoyo:atProduct" $ atProduct 0 $ pTrace "yoyo:ctx" ctx)
      $ \txInfo ->
        plet (atProduct 7 $ pTrace "yoyo:txInfo" txInfo) $ \signatories' ->
          plet (pUnListData £ punsafeCoerce (pTrace "yoyo:sig" signatories')) $ \signatories ->
            plet (pTrace "yoyo:hasElem" $ hasElem (punsafeCoerce datum) signatories) $ \(isBeneficiary :: Term s PBool) ->
              pif isBeneficiary (pcon PUnit) $ pTrace "yoyo:not-beneficiary" perror

instance PEq POpaque where
  a £== b =
    pBuiltin PLC.EqualsData nat0 £ a £ b

hasElem :: forall s. Term s POpaque -> Term s POpaque -> Term s PBool
hasElem k list' =
  go £ list'
  where
    -- TODO: why can't i use phoistAcyclic  here?
    go :: Term s (POpaque :--> PBool)
    go =
      pfix £$ plam $ \self list ->
        pmatch' list $ \case
          PNil -> pTrace "yoyo:hasElem:error" $ pcon PFalse
          PCons x xs ->
            pif
              (k £== x)
              (pcon PTrue)
              (self £ xs)

atProduct :: Term s PInteger -> Term s a -> Term s POpaque
atProduct n dat =
  plet (pTrace "yoyo:pUnConstrData" $ pUnConstrData £ punsafeCoerce dat) $ \dat' ->
    pmatch' (pTrace "yoyo:dat'" dat') $ \(PPair _ products :: PPair s) ->
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
          pmatch' (pTrace "yoyo:n" list) $ \case
            PNil -> pTrace "yoyo:atIndex:err" perror
            PCons x xs ->
              pif
                (n' £== 0)
                x
                (self £ (n' - 1) £ xs)

-- Returns a pair of Integer and [Data]
pUnConstrData :: Term s (POpaque :--> POpaque)
pUnConstrData = pBuiltin PLC.UnConstrData nat0

pUnListData :: Term s (POpaque :--> POpaque)
pUnListData = pBuiltin PLC.UnListData nat0

pBuiltin :: PLC.DefaultFun -> Nat -> Term s a
pBuiltin builtin forceLevel =
  phoistAcyclic $ forceN forceLevel $ punsafeBuiltin builtin
  where
    forceN :: Nat -> Term s a -> Term s a
    forceN Z = id
    forceN (S n) = pforce . punsafeCoerce . forceN n

-- All of these are working with `Data`

-- This is all opaque now, but we want to make it typed.
data PPair s = PPair (Term s POpaque) (Term s POpaque)

instance PlutusType PPair where
  type PInner PPair _ = POpaque
  pcon' (PPair a b) = pMkPairData £ a £ b
  pmatch' pair f =
    -- TODO: use delay/force to avoid evaluating `pair` twice?
    plet (pFstPair £ pair) $ \a ->
      plet (pSndPair £ pair) $ \b ->
        f $ PPair a b

pMkPairData :: Term s (POpaque :--> POpaque :--> POpaque)
pMkPairData =
  pBuiltin PLC.MkPairData nat0

pFstPair :: Term s (POpaque :--> POpaque)
pFstPair = pBuiltin PLC.FstPair nat2

pSndPair :: Term s (POpaque :--> POpaque)
pSndPair = pBuiltin PLC.SndPair nat2

data PList s
  = PNil
  | PCons (Term s POpaque) (Term s POpaque)

instance PlutusType PList where
  type PInner PList _ = POpaque
  pcon' PNil = undefined -- TODO??
  pcon' (PCons x xs) = pMkCons £ x £ xs
  pmatch' list f =
    plet (pNullList £ list) $ \isEmpty ->
      pif
        (punsafeCoerce isEmpty)
        (f PNil)
        $ plet
          (pHeadList £ list)
          ( \head ->
              plet (pTailList £ list) $ \tail ->
                f $ PCons head tail
          )

pMkCons :: Term s (POpaque :--> POpaque :--> POpaque)
pMkCons = pBuiltin PLC.MkCons nat1

pNullList :: Term s (POpaque :--> PBool)
pNullList = pBuiltin PLC.NullList nat1

pHeadList :: Term s (POpaque :--> POpaque)
pHeadList = pBuiltin PLC.HeadList nat1

pTailList :: Term s (POpaque :--> POpaque)
pTailList = pBuiltin PLC.TailList nat1

pTrace :: forall a s. Text -> Term s a -> Term s a
pTrace s f = pBuiltin PLC.Trace nat1 £ pfromText s £ f
