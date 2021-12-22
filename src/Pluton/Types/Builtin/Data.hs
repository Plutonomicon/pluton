{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pluton.Types.Builtin.Data
  ( PData (..),
    mkPairData,
  )
where

import Data.Data (Proxy (Proxy))
import Data.Type.Nat
import Plutarch
import Plutarch.Bool (PBool, PEq (..))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude (Type)
import Pluton.Types.Builtin
import Pluton.Types.Builtin qualified as B
import Pluton.Types.Builtin.List
import Pluton.Types.Builtin.Pair
import PlutusCore.Data qualified as PLC
import PlutusCore.Default qualified as PLC

data PData s
  = PDataConstr (Term s (PPair PInteger (PList PData)))
  | PDataMap (Term s (PList (PPair PData PData)))
  | PDataList (Term s (PList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)

type instance PBuiltinType 'PLC.ConstrData '[] = PPair PInteger (PList PData) :--> PData

type instance PBuiltinForce 'PLC.ConstrData = Nat0

type instance PBuiltinType 'PLC.MapData '[] = PList (PPair PData PData) :--> PData

type instance PBuiltinForce 'PLC.MapData = Nat0

type instance PBuiltinType 'PLC.ListData '[] = PList PData :--> PData

type instance PBuiltinForce 'PLC.ListData = Nat0

type instance PBuiltinType 'PLC.IData '[] = PInteger :--> PData

type instance PBuiltinForce 'PLC.IData = Nat0

type instance PBuiltinType 'PLC.BData '[] = PByteString :--> PData

type instance PBuiltinForce 'PLC.BData = Nat0

type instance
  PBuiltinType 'PLC.ChooseData '[PDelayed c] =
    PData
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c

type instance PBuiltinForce 'PLC.ChooseData = Nat1

type instance PBuiltinType 'PLC.UnConstrData '[] = PData :--> PPair PInteger (PList PData)

type instance PBuiltinForce 'PLC.UnConstrData = Nat0

type instance PBuiltinType 'PLC.UnMapData '[] = PData :--> PList (PPair PData PData)

type instance PBuiltinForce 'PLC.UnMapData = Nat0

type instance PBuiltinType 'PLC.UnListData '[] = PData :--> PList PData

type instance PBuiltinForce 'PLC.UnListData = Nat0

type instance PBuiltinType 'PLC.UnIData '[] = PData :--> PInteger

type instance PBuiltinForce 'PLC.UnIData = Nat0

type instance PBuiltinType 'PLC.UnBData '[] = PData :--> PByteString

type instance PBuiltinForce 'PLC.UnBData = Nat0

type instance PBuiltinType 'PLC.EqualsData '[] = PData :--> PData :--> PBool

type instance PBuiltinForce 'PLC.EqualsData = Nat0

instance PlutusType PData where
  type PInner PData _ = PData
  pcon' = \case
    PDataConstr pair ->
      B.pBuiltin @'PLC.ConstrData @'[] £ pair
    PDataMap m ->
      B.pBuiltin @'PLC.MapData @'[] £ m
    PDataList l ->
      B.pBuiltin @'PLC.ListData @'[] £ l
    PDataInteger i ->
      B.pBuiltin @'PLC.IData @'[] £ i
    PDataByteString bs ->
      B.pBuiltin @'PLC.BData @'[] £ bs
  pmatch' :: forall s c. (forall b. Term s (PInner PData b)) -> (PData s -> Term s c) -> Term s c
  pmatch' x f =
    pforce $
      B.pBuiltin @'PLC.ChooseData @'[PDelayed c] £ x
        £ pdelay (f (PDataConstr (B.pBuiltin @'PLC.UnConstrData @'[] £ x)))
        £ pdelay (f (PDataMap (B.pBuiltin @'PLC.UnMapData @'[] £ x)))
        £ pdelay (f (PDataList (B.pBuiltin @'PLC.UnListData @'[] £ x)))
        £ pdelay (f (PDataInteger (B.pBuiltin @'PLC.UnIData @'[] £ x)))
        £ pdelay (f (PDataByteString (B.pBuiltin @'PLC.UnBData @'[] £ x)))

instance ListElemUni PData where
  type ListElemType PData = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData

instance PEq PData where
  a £== b = B.pBuiltin @'PLC.EqualsData @'[] £ a £ b

-- This instance is for Data only, because `MkPairData` is the only way to
-- construct a pair. If you want to use a polymorphic pair, use `matchPair`
-- directly.
instance (a ~ PData, b ~ PData) => PlutusType (PPair a b) where
  type PInner (PPair a b) _ = PPair a b
  pcon' (PPair a b) =
    pBuiltin @'PLC.MkPairData @'[a, b] £ a £ b -- There is no MkPair
  pmatch' = matchPair

-- | Create a `Pair` of `Data` values.
mkPairData ::
  forall k (s :: k) (a :: k -> Type) (b :: k -> Type).
  (a ~ PData, b ~ PData) =>
  Term s a ->
  Term s b ->
  Term s (PPair a b)
mkPairData x y = pcon' $ PPair x y
