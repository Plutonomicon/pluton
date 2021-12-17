{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plut.Sample.Validator.Plutarch (plutarchValidator) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Ledger.Scripts (Validator (..))
import Plutarch
import Plutarch.Bool
import Plutarch.ByteString
import Plutarch.Integer
import Plutarch.Unit
import PlutusCore qualified as PLC

-- TODO: implement this
plutarchValidator :: Validator
plutarchValidator =
  Validator $ compile validator

validator :: forall s. Term s (PByteString :--> PByteString :--> PByteString :--> PUnit)
validator =
  plam $ \datum (_redeemer :: Term s PByteString) (ctx :: Term s PByteString) ->
    pTrace "yoyo" $
      plet
        (atProduct 0 ctx)
        $ \txInfo ->
          plet (atProduct 7 txInfo) $ \signatories' ->
            plet (pUnListData £ punsafeCoerce signatories') $ \signatories ->
              plet (hasElem (punsafeCoerce datum) signatories) $ \(isBeneficiary :: Term s PBool) ->
                pif isBeneficiary (pcon PUnit) perror

instance PEq POpaque where
  a £== b =
    pBuiltin PLC.EqualsData £ a £ b

hasElem :: forall s. Term s POpaque -> Term s POpaque -> Term s PBool
hasElem k list' =
  go £ list'
  where
    -- TODO: why can't i use phoistAcyclic  here?
    go :: Term s (POpaque :--> PBool)
    go =
      pfix £$ plam $ \self list ->
        pmatch' list $ \case
          PNil -> pcon PFalse
          PCons x xs ->
            pif
              (k £== x)
              (pcon PTrue)
              (self £ xs)

atProduct :: Term s PInteger -> Term s a -> Term s POpaque
atProduct n dat =
  plet (pUnConstrData £ punsafeCoerce dat) $ \dat' ->
    pmatch' dat' $ \(PPair _ products :: PPair s) ->
      atIndex n products

atIndex :: Term s PInteger -> Term s POpaque -> Term s POpaque
atIndex n =
  (go £ (n - 1) £)
  where
    go :: Term s (PInteger :--> POpaque :--> POpaque)
    go = phoistAcyclic $
      pfix
        £$ plam
        $ \self n' list ->
          pmatch' list $ \case
            PNil -> perror
            PCons x xs ->
              pifB (n' £== 0)
                £ x
                £ (self £ (n' - 1) £ xs)

-- | Builtin IfThenElse (strict, not lazy like pmatch)
pifB :: Term s PBool -> Term s (a :--> a :--> a)
pifB = (pif' £)

-- Returns a pair of Integer and [Data]
pUnConstrData :: Term s (POpaque :--> POpaque)
pUnConstrData = pBuiltin PLC.UnConstrData

pUnListData :: Term s (POpaque :--> POpaque)
pUnListData = pBuiltin PLC.UnListData

pBuiltin :: PLC.DefaultFun -> Term s a
pBuiltin builtin =
  phoistAcyclic $ pforce $ punsafeBuiltin builtin

-- All of these are working with `Data`

-- This is all opaque now, but we want to make it typed.
data PPair s = PPair (Term s POpaque) (Term s POpaque)

instance PlutusType PPair where
  type PInner PPair _ = POpaque
  pcon' (PPair a b) = pMkPairData £ a £ b
  pmatch' pair f =
    plet (pFstPair £ pair) $ \a ->
      plet (pSndPair £ pair) $ \b ->
        f $ PPair a b

pMkPairData :: Term s (POpaque :--> POpaque :--> POpaque)
pMkPairData =
  pBuiltin PLC.MkPairData

pFstPair :: Term s (POpaque :--> POpaque)
pFstPair = pBuiltin PLC.FstPair

pSndPair :: Term s (POpaque :--> POpaque)
pSndPair = pBuiltin PLC.SndPair

data PList s
  = PNil
  | PCons (Term s POpaque) (Term s POpaque)

instance PlutusType PList where
  type PInner PList _ = POpaque
  pcon' PNil = undefined -- TODO??
  pcon' (PCons x xs) = pMkCons £ x £ xs
  pmatch' list f =
    plet (pChooseList £ list £ punsafeCoerce (pcon' PTrue) £ punsafeCoerce (pcon' PTrue)) $ \isEmpty ->
      pifB (punsafeCoerce isEmpty)
        £ f PNil
        £ plet
          (pHeadList £ list)
          ( \head ->
              plet (pTailList £ list) $ \tail ->
                f $ PCons head tail
          )

pMkCons :: Term s (POpaque :--> POpaque :--> POpaque)
pMkCons = pBuiltin PLC.MkCons

pHeadList :: Term s (POpaque :--> POpaque)
pHeadList = pBuiltin PLC.HeadList

pTailList :: Term s (POpaque :--> POpaque)
pTailList = pBuiltin PLC.TailList

pChooseList :: Term s (POpaque :--> POpaque :--> POpaque :--> POpaque)
pChooseList = pBuiltin PLC.ChooseList

pTrace :: forall a s. Text -> Term s a -> Term s a
pTrace s f = pBuiltin PLC.Trace £ pByteString (encodeUtf8 s) £ f

pByteString :: ByteString -> Term s PByteString
pByteString =
  punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniByteString
