{-# LANGUAGE MultiParamTypeClasses #-}

module Pluton.Types.Builtin.Pair
  ( PPair (..),
    matchPair,
    fstPair,
    sndPair,
  )
where

import Data.Type.Nat
import Plutarch
import Plutarch.Prelude
import Pluton.Types.Builtin
import PlutusCore qualified as PLC

-- | A builtin pair type.
data PPair a b s = PPair (Term s a) (Term s b)

type instance PBuiltinType 'PLC.MkPairData '[a, b] = a :--> b :--> PPair a b

type instance PBuiltinForce 'PLC.MkPairData = Nat2

type instance PBuiltinType 'PLC.FstPair '[a, b] = PPair a b :--> a

type instance PBuiltinForce 'PLC.FstPair = Nat2

type instance PBuiltinType 'PLC.SndPair '[a, b] = PPair a b :--> b

type instance PBuiltinForce 'PLC.SndPair = Nat2

-- | Match on a polymorphic pair of values
matchPair ::
  forall a b s c.
  Term s (PPair a b) ->
  (PPair a b s -> Term s c) ->
  Term s c
matchPair pair f =
  -- TODO: use delay/force to avoid evaluating `pair` twice?
  plet (fstPair £ pair) $ \a ->
    plet (sndPair £ pair) $ \b ->
      f $ PPair a b

fstPair :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). Term s (PPair a b :--> a)
fstPair = pBuiltin @'PLC.FstPair @'[a, b]

sndPair :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). Term s (PPair a b :--> b)
sndPair = pBuiltin @'PLC.SndPair @'[a, b]
