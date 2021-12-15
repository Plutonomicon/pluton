{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

module Plut.Sample.Validator.Haskell (haskellValidator) where

import Ledger (Validator, mkValidatorScript)
import Ledger.Contexts
  ( ScriptContext (scriptContextTxInfo),
    txSignedBy,
  )
import PlutusTx qualified
import PlutusTx.Prelude

-- -------------------------------------------------------------------------- --
-- Onchain (Haskell)                                                          --
-- -------------------------------------------------------------------------- --

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator
  (PlutusTx.unsafeFromBuiltinData -> pkh)
  _
  (PlutusTx.unsafeFromBuiltinData -> ctx) =
    if traceIfFalse "beneficiary's signature missing" (scriptContextTxInfo ctx `txSignedBy` pkh)
      then ()
      else error ()

haskellValidator :: Validator
haskellValidator = mkValidatorScript $$(PlutusTx.compile [||mkValidator||])
