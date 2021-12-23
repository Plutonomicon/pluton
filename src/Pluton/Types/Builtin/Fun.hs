{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Type-safe builtins.
--
-- To define the type of a particular builtin, write a type instance for
-- `PBuiltinFunType`. This enables `pBuiltinFun` to be used on that PLC builtin:
--
-- @(pBuiltinFun @'PLC.UnIData @'[] # someData)@
module Pluton.Types.Builtin.Fun
  ( pBuiltinFun,
    PBuiltinFunType,
  )
where

import Data.Type.Nat
import Plutarch
import Plutarch.Prelude
import Plutarch.String
import PlutusCore qualified as PLC

-- This class exists only because Haskell has no way to get the value given the
-- promoted data kind type.
class PBuiltinFun (builtin :: PLC.DefaultFun) as where
  pBuiltinFunVal :: PLC.DefaultFun

type family PBuiltinFunType (builtin :: PLC.DefaultFun) (as :: [k -> Type]) :: k -> Type

type family Length (l :: [k]) :: Nat

type instance Length '[] = 'Z

type instance Length (x : xs) = 'S (Length xs)

-- | Type-safe version of `punsafeBuiltin`
--
-- Use as: pBuiltinFun @'PLC.AddInteger @'[]
--
-- The second type argument is the list of polymorphic vars in the builtin.
pBuiltinFun ::
  forall builtin as s.
  (PBuiltinFun builtin as, SNatI (Length as)) =>
  Term s (PBuiltinFunType builtin as)
pBuiltinFun =
  force . punsafeBuiltin $ pBuiltinFunVal @builtin @as
  where
    -- The number of forces to apply is equivalent to the arity of polymorphic
    -- vars `as`.
    force :: Term s b -> Term s b
    force =
      let sn = snat :: SNat (Length as)
       in forceN (snatToNat sn)
    forceN :: forall s a. Nat -> Term s a -> Term s a
    forceN Z = id
    forceN (S n) = pforce . punsafeCoerce . forceN n

-- Instances

instance PBuiltinFun 'PLC.MkCons a where
  pBuiltinFunVal = PLC.MkCons

instance PBuiltinFun 'PLC.NullList a where
  pBuiltinFunVal = PLC.NullList

instance PBuiltinFun 'PLC.HeadList a where
  pBuiltinFunVal = PLC.HeadList

instance PBuiltinFun 'PLC.TailList a where
  pBuiltinFunVal = PLC.TailList

instance PBuiltinFun 'PLC.MkPairData a where
  pBuiltinFunVal = PLC.MkPairData

instance PBuiltinFun 'PLC.FstPair a where
  pBuiltinFunVal = PLC.FstPair

instance PBuiltinFun 'PLC.SndPair a where
  pBuiltinFunVal = PLC.SndPair

instance PBuiltinFun 'PLC.ConstrData a where
  pBuiltinFunVal = PLC.ConstrData

instance PBuiltinFun 'PLC.MapData a where
  pBuiltinFunVal = PLC.MapData

instance PBuiltinFun 'PLC.ListData a where
  pBuiltinFunVal = PLC.ListData

instance PBuiltinFun 'PLC.IData a where
  pBuiltinFunVal = PLC.IData

instance PBuiltinFun 'PLC.BData a where
  pBuiltinFunVal = PLC.BData

instance PBuiltinFun 'PLC.ChooseData a where
  pBuiltinFunVal = PLC.ChooseData

instance PBuiltinFun 'PLC.EqualsData a where
  pBuiltinFunVal = PLC.EqualsData

instance PBuiltinFun 'PLC.UnConstrData a where
  pBuiltinFunVal = PLC.UnConstrData

instance PBuiltinFun 'PLC.UnMapData a where
  pBuiltinFunVal = PLC.UnMapData

instance PBuiltinFun 'PLC.UnListData a where
  pBuiltinFunVal = PLC.UnListData

instance PBuiltinFun 'PLC.UnIData a where
  pBuiltinFunVal = PLC.UnIData

instance PBuiltinFun 'PLC.UnBData a where
  pBuiltinFunVal = PLC.UnBData

instance PBuiltinFun 'PLC.Trace a where
  pBuiltinFunVal = PLC.Trace

type instance PBuiltinFunType 'PLC.Trace '[a] = PString :--> a :--> a
