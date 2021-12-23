module Pluton.Types.Builtin.Uni (PDefaultUni (..)) where

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
-- We use this in: PLC.knownUniOf $ Proxy @(PDefaultUniType a)
class PLC.DefaultUni `PLC.Contains` PDefaultUniType a => PDefaultUni (a :: k -> Type) where
  type PDefaultUniType a :: Type

instance PDefaultUni PInteger where
  type PDefaultUniType PInteger = Integer

instance PDefaultUni PString where
  type PDefaultUniType PString = Text

instance PDefaultUni PByteString where
  type PDefaultUniType PByteString = ByteString

instance PDefaultUni PBool where
  type PDefaultUniType PBool = Bool

instance PDefaultUni PUnit where
  type PDefaultUniType PUnit = ()

instance PDefaultUni POpaque where
  type PDefaultUniType POpaque = PLC.Data
