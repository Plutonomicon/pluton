module Pluton.Types.Builtin.Data.Type (
  PData (..),
) where

import Plutarch
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Pluton.Types.Builtin.List.Type
import Pluton.Types.Builtin.Pair.Type

data PData s
  = PDataConstr (Term s (PPair PInteger (PList PData)))
  | PDataMap (Term s (PList (PPair PData PData)))
  | PDataList (Term s (PList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)
