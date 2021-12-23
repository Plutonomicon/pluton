{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pluton.Types.Builtin.Data
  ( PData (..),
    mkPairData,
  )
where

import Plutarch
import Plutarch.Bool (PBool, PEq (..))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Pluton.Types.Builtin
import Pluton.Types.Builtin qualified as B
import Pluton.Types.Builtin.List
import Pluton.Types.Builtin.Pair
import Pluton.Types.Builtin.Uni
import PlutusCore.Data qualified as PLC
import PlutusCore.Default qualified as PLC

data PData s
  = PDataConstr (Term s (PPair PInteger (PList PData)))
  | PDataMap (Term s (PList (PPair PData PData)))
  | PDataList (Term s (PList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)

type instance PBuiltinFunType 'PLC.ConstrData '[] = PPair PInteger (PList PData) :--> PData

type instance PBuiltinFunType 'PLC.MapData '[] = PList (PPair PData PData) :--> PData

type instance PBuiltinFunType 'PLC.ListData '[] = PList PData :--> PData

type instance PBuiltinFunType 'PLC.IData '[] = PInteger :--> PData

type instance PBuiltinFunType 'PLC.BData '[] = PByteString :--> PData

type instance
  PBuiltinFunType 'PLC.ChooseData '[PDelayed c] =
    PData
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c

type instance PBuiltinFunType 'PLC.UnConstrData '[] = PData :--> PPair PInteger (PList PData)

type instance PBuiltinFunType 'PLC.UnMapData '[] = PData :--> PList (PPair PData PData)

type instance PBuiltinFunType 'PLC.UnListData '[] = PData :--> PList PData

type instance PBuiltinFunType 'PLC.UnIData '[] = PData :--> PInteger

type instance PBuiltinFunType 'PLC.UnBData '[] = PData :--> PByteString

type instance PBuiltinFunType 'PLC.EqualsData '[] = PData :--> PData :--> PBool

instance PlutusType PData where
  type PInner PData _ = PData
  pcon' = \case
    PDataConstr pair ->
      B.pBuiltinFun @'PLC.ConstrData @'[] # pair
    PDataMap m ->
      B.pBuiltinFun @'PLC.MapData @'[] # m
    PDataList l ->
      B.pBuiltinFun @'PLC.ListData @'[] # l
    PDataInteger i ->
      B.pBuiltinFun @'PLC.IData @'[] # i
    PDataByteString bs ->
      B.pBuiltinFun @'PLC.BData @'[] # bs
  pmatch' :: forall s c. (forall b. Term s (PInner PData b)) -> (PData s -> Term s c) -> Term s c
  pmatch' x f =
    pforce $
      B.pBuiltinFun @'PLC.ChooseData @'[PDelayed c] # x
        # pdelay (f (PDataConstr (B.pBuiltinFun @'PLC.UnConstrData @'[] # x)))
        # pdelay (f (PDataMap (B.pBuiltinFun @'PLC.UnMapData @'[] # x)))
        # pdelay (f (PDataList (B.pBuiltinFun @'PLC.UnListData @'[] # x)))
        # pdelay (f (PDataInteger (B.pBuiltinFun @'PLC.UnIData @'[] # x)))
        # pdelay (f (PDataByteString (B.pBuiltinFun @'PLC.UnBData @'[] # x)))

instance PDefaultFun PData where
  type PDefaultFunType PData = PLC.Data

instance PEq PData where
  a #== b = B.pBuiltinFun @'PLC.EqualsData @'[] # a # b

-- This instance is for Data only, because `MkPairData` is the only way to
-- construct a pair. If you want to use a polymorphic pair, use `matchPair`
-- directly.
instance PlutusType (PPair PData PData) where
  type PInner (PPair PData PData) _ = PPair PData PData
  pcon' (PPair a b) =
    pBuiltinFun @'PLC.MkPairData @'[] # a # b -- There is no MkPair
  pmatch' = matchPair

type instance PBuiltinFunType 'PLC.MkPairData '[] = PData :--> PData :--> PPair PData PData

-- | Create a `Pair` of `Data` values.
mkPairData :: forall k (s :: k). Term s PData -> Term s PData -> Term s (PPair PData PData)
mkPairData x y = pBuiltinFun @'PLC.MkPairData @'[] # x # y
