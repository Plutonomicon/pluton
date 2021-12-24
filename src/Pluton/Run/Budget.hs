-- | Execution budget and script size for Plutus scripts
module Pluton.Run.Budget
  ( Budget (..),
    -- | * Budget for an arbitraty Plutus script
    scriptBudget,
    -- | * Budget for a smart contract validator script
    validatorBudget,
    -- | * Budget for EmulatorTrace
    emulatorTraceBudget,
  )
where

import Codec.Serialise (serialise)
import Control.Arrow ((&&&))
import Control.Foldl qualified as Foldl
import Control.Monad.Freer qualified as Freer
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Data.Default (Default (def))
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Flat (flat)
import GHC.Stack (HasCallStack)
import Ledger.Index (ExBudget, ScriptValidationEvent (..), ValidatorMode (FullyAppliedValidators), getScript)
import Ledger.Scripts (Script (unScript), Validator)
import Plutus.Trace.Emulator qualified as Em
import Plutus.V1.Ledger.Api qualified as Plutus
import Streaming.Prelude qualified as S
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream (foldEmulatorStreamM)

data Budget = Budget
  { exBudget :: ExBudget,
    scriptSizeBytes :: Int64
  }
  deriving stock (Show)

-- | Return the exbudget and script size of the *applied* validator run inside
-- an Emulator trace.
--
-- The trace must have run the validation exactly once, else this will fail. We
-- do this, because we are benchmarking a single run of the validator, not
-- multiple runs.
emulatorTraceBudget :: Em.EmulatorTrace a -> Budget
emulatorTraceBudget trace =
  -- Most of the code here is taken from `Plutus.Trace.Emulator.Extract` (IOHK
  -- doesn't care to export it).
  let stream = Em.runEmulatorStream def trace
      getEvents :: Folds.EmulatorEventFold a -> a
      getEvents theFold = S.fst' $ Freer.run $ foldEmulatorStreamM (Foldl.generalize theFold) stream
      -- This gets us the fully applied validator script.
      -- Note: This doesn't deal with minting policy scripts
      mode = FullyAppliedValidators
      f event@ScriptValidationEvent {sveResult} =
        let bytes = BSL.fromStrict . flat . unScript . getScript mode $ event
            byteSize = BSL.length bytes
            exBudget = either (error . show) fst sveResult
         in Budget exBudget byteSize
   in f . exactlyOne $ getEvents Folds.scriptEvents
  where
    exactlyOne :: [a] -> a
    exactlyOne [x] = x
    exactlyOne _ = error "benchEmulatorTrace: expected exactly one validator run"

-- | Return the script size in bytes along with execution budget.
--
-- NOTE: This calculates the budget for an *applied* validator. Use
-- @emulatorTraceBudget@ if you want to calculate on applied validators.
validatorBudget :: Validator -> Budget
validatorBudget = uncurry Budget . (evalScriptCounting &&& (fromInteger . toInteger . SBS.length)) . serialiseValidator

scriptBudget :: Script -> Budget
scriptBudget = uncurry Budget . (evalScriptCounting &&& (fromInteger . toInteger . SBS.length)) . serialiseScript

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
