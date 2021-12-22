{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pluton.Types.Builtin
  ( pBuiltin,
    PBuiltin (..),
    PBuiltinType,
    PBuiltinForce,
    (!#),
  )
where

import Data.Text (Text)
import Data.Type.Nat
import Plutarch
import Plutarch.Prelude
import Plutarch.String
import PlutusCore qualified as PLC

class PBuiltin (builtin :: PLC.DefaultFun) a where
  pBuiltinVal :: PLC.DefaultFun

type family PBuiltinType (builtin :: PLC.DefaultFun) (as :: [k -> Type]) :: k -> Type

-- TODO: Automatically calculate this from `as` of PBuiltinType.
type family PBuiltinForce (builtin :: PLC.DefaultFun) :: Nat

-- | Type-safe version of `punsafeBuiltin` 
--
-- Use as: pBuiltin @'PLC.AddInteger @'[] 
--
-- The second type argument is the list of polymorphic vars in the builtin.
pBuiltin ::
  forall builtin a s.
  (PBuiltin builtin a, SNatI (PBuiltinForce builtin)) =>
  Term s (PBuiltinType builtin a)
pBuiltin =
  force . punsafeBuiltin $ pBuiltinVal @builtin @a
  where
    force :: Term s b -> Term s b
    force =
      let sn = snat :: SNat (PBuiltinForce builtin)
       in forceN (snatToNat sn)
    forceN :: forall s a. Nat -> Term s a -> Term s a
    forceN Z = id
    forceN (S n) = pforce . punsafeCoerce . forceN n

pTrace :: forall a s. Text -> Term s a -> Term s a
pTrace s f = pBuiltin @('PLC.Trace) @'[a] £ pfromText s £ f

(!#) :: forall k (s :: k) (a :: k -> Type). Text -> Term s a -> Term s a
(!#) = pTrace

infixl 8 !#

-- Instances

instance PBuiltin 'PLC.MkCons a where
  pBuiltinVal = PLC.IData

instance PBuiltin 'PLC.NullList a where
  pBuiltinVal = PLC.NullList

instance PBuiltin 'PLC.HeadList a where
  pBuiltinVal = PLC.HeadList

instance PBuiltin 'PLC.TailList a where
  pBuiltinVal = PLC.TailList

instance PBuiltin 'PLC.MkPairData a where
  pBuiltinVal = PLC.MkPairData

instance PBuiltin 'PLC.FstPair a where
  pBuiltinVal = PLC.FstPair

instance PBuiltin 'PLC.SndPair a where
  pBuiltinVal = PLC.SndPair

instance PBuiltin 'PLC.ConstrData a where
  pBuiltinVal = PLC.ConstrData

instance PBuiltin 'PLC.MapData a where
  pBuiltinVal = PLC.MapData

instance PBuiltin 'PLC.ListData a where
  pBuiltinVal = PLC.ListData

instance PBuiltin 'PLC.IData a where
  pBuiltinVal = PLC.IData

instance PBuiltin 'PLC.BData a where
  pBuiltinVal = PLC.BData

instance PBuiltin 'PLC.ChooseData a where
  pBuiltinVal = PLC.ChooseData

instance PBuiltin 'PLC.EqualsData a where
  pBuiltinVal = PLC.EqualsData

instance PBuiltin 'PLC.UnConstrData a where
  pBuiltinVal = PLC.UnConstrData

instance PBuiltin 'PLC.UnMapData a where
  pBuiltinVal = PLC.UnMapData

instance PBuiltin 'PLC.UnListData a where
  pBuiltinVal = PLC.UnListData

instance PBuiltin 'PLC.UnIData a where
  pBuiltinVal = PLC.UnIData

instance PBuiltin 'PLC.UnBData a where
  pBuiltinVal = PLC.UnBData

instance PBuiltin 'PLC.Trace a where
  pBuiltinVal = PLC.Trace

type instance PBuiltinType 'PLC.Trace '[a] = PString :--> a :--> a

type instance PBuiltinForce 'PLC.Trace = Nat0

