{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pluton.Types.Builtin
  ( ppBuiltin,
    ppBuiltin0,
    PPBuiltin (..),
    PPBuiltinType,
    PPBuiltinForce,
    (!#),
  )
where

import Data.Text (Text)
import Data.Type.Nat
import Plutarch
import Plutarch.Prelude
import Plutarch.String
import PlutusCore qualified as PLC

class PPBuiltin (builtin :: PLC.DefaultFun) a where
  ppBuiltinVal :: PLC.DefaultFun

type family PPBuiltinType (builtin :: PLC.DefaultFun) (as :: [k -> Type]) :: k -> Type

type family PPBuiltinForce (builtin :: PLC.DefaultFun) :: Nat

instance PPBuiltin 'PLC.MkCons a where
  ppBuiltinVal = PLC.IData

instance PPBuiltin 'PLC.NullList a where
  ppBuiltinVal = PLC.NullList

instance PPBuiltin 'PLC.HeadList a where
  ppBuiltinVal = PLC.HeadList

instance PPBuiltin 'PLC.TailList a where
  ppBuiltinVal = PLC.TailList

instance PPBuiltin 'PLC.MkPairData a where
  ppBuiltinVal = PLC.MkPairData

instance PPBuiltin 'PLC.FstPair a where
  ppBuiltinVal = PLC.FstPair

instance PPBuiltin 'PLC.SndPair a where
  ppBuiltinVal = PLC.SndPair

instance PPBuiltin 'PLC.ConstrData a where
  ppBuiltinVal = PLC.ConstrData

instance PPBuiltin 'PLC.MapData a where
  ppBuiltinVal = PLC.MapData

instance PPBuiltin 'PLC.ListData a where
  ppBuiltinVal = PLC.ListData

instance PPBuiltin 'PLC.IData a where
  ppBuiltinVal = PLC.IData

instance PPBuiltin 'PLC.BData a where
  ppBuiltinVal = PLC.BData

instance PPBuiltin 'PLC.ChooseData a where
  ppBuiltinVal = PLC.ChooseData

instance PPBuiltin 'PLC.EqualsData a where
  ppBuiltinVal = PLC.EqualsData

instance PPBuiltin 'PLC.UnConstrData a where
  ppBuiltinVal = PLC.UnConstrData

instance PPBuiltin 'PLC.UnMapData a where
  ppBuiltinVal = PLC.UnMapData

instance PPBuiltin 'PLC.UnListData a where
  ppBuiltinVal = PLC.UnListData

instance PPBuiltin 'PLC.UnIData a where
  ppBuiltinVal = PLC.UnIData

instance PPBuiltin 'PLC.UnBData a where
  ppBuiltinVal = PLC.UnBData

instance PPBuiltin 'PLC.Trace a where
  ppBuiltinVal = PLC.Trace

type instance PPBuiltinType 'PLC.Trace '[a] = PString :--> a :--> a

type instance PPBuiltinForce 'PLC.Trace = Nat0

pTrace :: forall a s. Text -> Term s a -> Term s a
pTrace s f = ppBuiltin @('PLC.Trace) @'[a] £ pfromText s £ f

(!#) :: forall k (s :: k) (a :: k -> Type). Text -> Term s a -> Term s a
(!#) = pTrace

infixl 8 !#

ppBuiltin ::
  forall builtin a s.
  (PPBuiltin builtin a, SNatI (PPBuiltinForce builtin)) =>
  Term s (PPBuiltinType builtin a)
ppBuiltin =
  force . punsafeBuiltin $ ppBuiltinVal @builtin @a
  where
    force :: Term s b -> Term s b
    force =
      let sn = snat :: SNat (PPBuiltinForce builtin)
       in forceN (snatToNat sn)
    forceN :: forall s a. Nat -> Term s a -> Term s a
    forceN Z = id
    forceN (S n) = pforce . punsafeCoerce . forceN n

ppBuiltin0 ::
  forall builtin a s.
  (a ~ '[], PPBuiltin builtin a, SNatI (PPBuiltinForce builtin)) =>
  Term s (PPBuiltinType builtin a)
ppBuiltin0 = ppBuiltin @builtin @'[]
