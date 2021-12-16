{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Plut.Sample.Validator.Pluto (plutoValidator) where

import Ledger.Scripts (Validator (..))
import PlutusCore.Assembler.Assemble qualified as Pluto
import PlutusCore.Assembler.FFI qualified as PlutoFFI
import PlutusCore.Assembler.Types.AST qualified as Pluto

plutoValidatorProg :: Pluto.Program ()
plutoValidatorProg = $(PlutoFFI.load "src/Plut/Sample/Validator/validator.pluto")

plutoValidator :: Validator
plutoValidator =
  case Pluto.translate plutoValidatorProg of
    -- TODO: fail in TH instead of at (offchain) runtime
    Left err -> Prelude.error $ Prelude.show err
    Right script -> Validator script
