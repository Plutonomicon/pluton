{-# LANGUAGE TemplateHaskell #-}

module Pluton.Sample.Validator.Pluto (plutoValidator) where

import Ledger.Scripts (Validator (..))
import PlutusCore.Assembler.Assemble qualified as Pluto
import PlutusCore.Assembler.FFI qualified as PlutoFFI
import PlutusCore.Assembler.Types.AST qualified as Pluto

-- FIXME: This is known to trigger a HLS bug with symbol resolution
-- See https://github.com/haskell/haskell-language-server/issues/1737#issuecomment-825516365
plutoValidatorProg :: Pluto.Program ()
plutoValidatorProg = $(PlutoFFI.load "src/Pluton/Sample/Validator/validator.pluto")

plutoValidator :: Validator
plutoValidator =
  case Pluto.translate plutoValidatorProg of
    -- TODO: fail in TH instead of at (offchain) runtime
    Left err -> Prelude.error $ Prelude.show err
    Right script -> Validator script
