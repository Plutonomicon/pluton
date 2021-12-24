module Pluton.Run.ScriptSize
  ( scriptSize,
    validatorSize,
  )
where

import Codec.Serialise (serialise)
import Control.Arrow ((&&&))
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import Ledger.Scripts (Script, Validator)
import Plutus.V1.Ledger.Api qualified as Plutus

-- (ExBudget {exBudgetCPU = ExCPU 327603, exBudgetMemory = ExMemory 1200},336)

-- | Return the script size in bytes along with execution budget.
validatorSize :: Validator -> (Plutus.ExBudget, Int)
validatorSize = (evalScriptCounting &&& SBS.length) . serialiseValidator

scriptSize :: Script -> (Plutus.ExBudget, Int)
scriptSize = (evalScriptCounting &&& SBS.length) . serialiseScript

serialiseScript :: Script -> SBS.ShortByteString
serialiseScript = SBS.toShort . LB.toStrict . serialise

serialiseValidator :: Validator -> SBS.ShortByteString
serialiseValidator = SBS.toShort . LB.toStrict . serialise

evalScriptCounting :: HasCallStack => Plutus.SerializedScript -> Plutus.ExBudget
evalScriptCounting script = do
  let costModel = fromJust Plutus.defaultCostModelParams
      (_logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose costModel script []
   in case e of
        Left evalErr -> error ("Eval Error: " <> show evalErr)
        Right exbudget -> exbudget
