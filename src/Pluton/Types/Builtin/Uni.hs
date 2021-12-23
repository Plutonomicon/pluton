module Pluton.Types.Builtin.Uni (PDefaultFun (..)) where

import Data.ByteString
import Data.Kind (Type)
import Data.Text (Text)
import Plutarch (POpaque)
import Plutarch.Bool
import Plutarch.ByteString
import Plutarch.Integer
import Plutarch.String
import Plutarch.Unit
import PlutusCore.Data qualified as PLC
import PlutusCore.Default qualified as PLC

-- | Class of eDSL Types that map to Plutus builtin in its `DefaultUni`
--
-- We use this in: PLC.knownUniOf $ Proxy @(PDefaultFunType a)
class PLC.DefaultUni `PLC.Contains` PDefaultFunType a => PDefaultFun (a :: k -> Type) where
  type PDefaultFunType a :: Type

instance PDefaultFun PInteger where
  type PDefaultFunType PInteger = Integer

instance PDefaultFun PString where
  type PDefaultFunType PString = Text

instance PDefaultFun PByteString where
  type PDefaultFunType PByteString = ByteString

instance PDefaultFun PBool where
  type PDefaultFunType PBool = Bool

instance PDefaultFun PUnit where
  type PDefaultFunType PUnit = ()

instance PDefaultFun POpaque where
  type PDefaultFunType POpaque = PLC.Data
