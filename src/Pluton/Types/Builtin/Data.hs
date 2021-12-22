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

type instance PPBuiltinType 'PLC.ConstrData '[] = PPair PInteger (PList PData) :--> PData

type instance PPBuiltinForce 'PLC.ConstrData = Nat0

type instance PPBuiltinType 'PLC.MapData '[] = PList (PPair PData PData) :--> PData

type instance PPBuiltinForce 'PLC.MapData = Nat0

type instance PPBuiltinType 'PLC.ListData '[] = PList PData :--> PData

type instance PPBuiltinForce 'PLC.ListData = Nat0

type instance PPBuiltinType 'PLC.IData '[] = PInteger :--> PData

type instance PPBuiltinForce 'PLC.IData = Nat0

type instance PPBuiltinType 'PLC.BData '[] = PByteString :--> PData

type instance PPBuiltinForce 'PLC.BData = Nat0

type instance
  PPBuiltinType 'PLC.ChooseData '[PDelayed c] =
    PData
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c
      :--> PDelayed c

type instance PPBuiltinForce 'PLC.ChooseData = Nat1

type instance PPBuiltinType 'PLC.UnConstrData '[] = PData :--> PPair PInteger (PList PData)

type instance PPBuiltinForce 'PLC.UnConstrData = Nat0

type instance PPBuiltinType 'PLC.UnMapData '[] = PData :--> PList (PPair PData PData)

type instance PPBuiltinForce 'PLC.UnMapData = Nat0

type instance PPBuiltinType 'PLC.UnListData '[] = PData :--> PList PData

type instance PPBuiltinForce 'PLC.UnListData = Nat0

type instance PPBuiltinType 'PLC.UnIData '[] = PData :--> PInteger

type instance PPBuiltinForce 'PLC.UnIData = Nat0

type instance PPBuiltinType 'PLC.UnBData '[] = PData :--> PByteString

type instance PPBuiltinForce 'PLC.UnBData = Nat0

type instance PPBuiltinType 'PLC.EqualsData '[] = PData :--> PData :--> PBool

type instance PPBuiltinForce 'PLC.EqualsData = Nat0

instance PlutusType PData where
  type PInner PData _ = PData
  pcon' = \case
    PDataConstr pair ->
      B.ppBuiltin0 @'PLC.ConstrData £ pair
    PDataMap m ->
      B.ppBuiltin0 @'PLC.MapData £ m
    PDataList l ->
      B.ppBuiltin0 @'PLC.ListData £ l
    PDataInteger i ->
      B.ppBuiltin0 @'PLC.IData £ i
    PDataByteString bs ->
      B.ppBuiltin0 @'PLC.BData £ bs
  pmatch' :: forall s c. (forall b. Term s (PInner PData b)) -> (PData s -> Term s c) -> Term s c
  pmatch' x f =
    pforce $
      B.ppBuiltin @'PLC.ChooseData @'[PDelayed c] £ x
        £ pdelay (f (PDataConstr (B.ppBuiltin0 @'PLC.UnConstrData £ x)))
        £ pdelay (f (PDataMap (B.ppBuiltin0 @'PLC.UnMapData £ x)))
        £ pdelay (f (PDataList (B.ppBuiltin0 @'PLC.UnListData £ x)))
        £ pdelay (f (PDataInteger (B.ppBuiltin0 @'PLC.UnIData £ x)))
        £ pdelay (f (PDataByteString (B.ppBuiltin0 @'PLC.UnBData £ x)))

instance ListElemUni PData where
  type ListElemType PData = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData

instance PEq PData where
  a £== b = B.ppBuiltin0 @'PLC.EqualsData £ a £ b

-- This instance is for Data only, because `MkPairData` is the only way to
-- construct a pair. If you want to use a polymorphic pair, use `matchPair`
-- directly.
instance (a ~ PData, b ~ PData) => PlutusType (PPair a b) where
  type PInner (PPair a b) _ = PPair a b
  pcon' (PPair a b) =
    ppBuiltin @'PLC.MkPairData @'[a, b] £ a £ b -- There is no MkPair
  pmatch' = matchPair

-- | Create a `Pair` of `Data` values.
mkPairData ::
  forall k (s :: k) (a :: k -> Type) (b :: k -> Type).
  (a ~ PData, b ~ PData) =>
  Term s a ->
  Term s b ->
  Term s (PPair a b)
mkPairData x y = pcon' $ PPair x y
