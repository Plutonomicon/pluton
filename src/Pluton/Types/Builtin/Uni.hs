module Pluton.Types.Builtin.Uni (PUni (..)) where

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
-- We use this in: PLC.knownUniOf $ Proxy @(PUniType a)
class PLC.DefaultUni `PLC.Contains` PUniType a => PUni (a :: k -> Type) where
  type PUniType a :: Type

instance PUni PInteger where
  type PUniType PInteger = Integer

instance PUni PString where
  type PUniType PString = Text

instance PUni PByteString where
  type PUniType PByteString = ByteString

instance PUni PBool where
  type PUniType PBool = Bool

instance PUni PUnit where
  type PUniType PUnit = ()

instance PUni POpaque where
  type PUniType POpaque = PLC.Data
