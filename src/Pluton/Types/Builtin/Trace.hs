module Pluton.Types.Builtin.Trace ((!#)) where

import Data.Text (Text)
import Plutarch
import Plutarch.Prelude
import Plutarch.String
import Pluton.Types.Builtin.Fun
import PlutusCore qualified as PLC

type instance PBuiltinFunType 'PLC.Trace '[a] = PString :--> a :--> a

(!#) :: forall k (s :: k) (a :: k -> Type). Text -> Term s a -> Term s a
(!#) s f = pBuiltinFun @'PLC.Trace @'[a] # pfromText s # f

infixl 8 !#
