module Pluton.Types.Builtin.Trace ((!#)) where

import Data.Text (Text)
import Plutarch
import Plutarch.Prelude
import Plutarch.String
import Pluton.Types.Builtin.Fun
import PlutusCore qualified as PLC

pTrace :: forall a s. Text -> Term s a -> Term s a
pTrace s f = pBuiltinFun @'PLC.Trace @'[a] # pfromText s # f

(!#) :: forall k (s :: k) (a :: k -> Type). Text -> Term s a -> Term s a
(!#) = pTrace

infixl 8 !#
