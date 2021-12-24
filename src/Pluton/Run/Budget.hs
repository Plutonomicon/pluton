{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- | Execution budget and script size for Plutus scripts
module Pluton.Run.Budget
  ( Budget (..),
    NamedBudget (..),
    -- | * Budget for an arbitraty Plutus script
    scriptBudget,
    -- | * Budget for EmulatorTrace
    emulatorTraceBudget,
  )
where

import Codec.Serialise qualified as Codec
import Control.Arrow ((&&&))
import Control.Foldl qualified as Foldl
import Control.Monad.Freer qualified as Freer
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Data.Csv
  ( DefaultOrdered (..),
    ToField,
    ToNamedRecord (..),
    header,
    namedRecord,
    (.=),
  )
import Data.Default (Default (def))
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Flat (flat)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Ledger (ExBudget (ExBudget))
import Ledger.Index (ExCPU, ExMemory, ScriptValidationEvent (..), ValidatorMode (FullyAppliedValidators), getScript)
import Ledger.Scripts (Script)
import Ledger.Scripts qualified as Scripts
import Plutus.Trace.Emulator qualified as Em
import Plutus.V1.Ledger.Api qualified as Plutus
import Streaming.Prelude qualified as S
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream (foldEmulatorStreamM)

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
        let bytes = BSL.fromStrict . flat . Scripts.unScript . getScript mode $ event
            byteSize = BSL.length bytes
            exBudget = either (error . show) fst sveResult
         in mkBudget exBudget byteSize
   in f . exactlyOne $ getEvents Folds.scriptEvents
  where
    exactlyOne :: [a] -> a
    exactlyOne [x] = x
    exactlyOne _ = error "benchEmulatorTrace: expected exactly one validator run"

scriptBudget :: Script -> Budget
scriptBudget = uncurry mkBudget . (evalScriptCounting &&& (fromInteger . toInteger . SBS.length)) . serialiseScript

serialiseScript :: Script -> SBS.ShortByteString
serialiseScript = SBS.toShort . LB.toStrict . Codec.serialise -- Using `flat` here breaks `evalScriptCounting`

evalScriptCounting :: HasCallStack => Plutus.SerializedScript -> Plutus.ExBudget
evalScriptCounting script = do
  let costModel = fromJust Plutus.defaultCostModelParams
      (_logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose costModel script []
   in case e of
        Left evalErr -> error ("Eval Error: " <> show evalErr)
        Right exbudget -> exbudget

--- Types

data Budget = Budget
  { exBudgetCPU :: ExCPU,
    exBudgetMemory :: ExMemory,
    scriptSizeBytes :: ScriptSizeBytes
  }
  deriving stock (Show, Generic)

newtype ScriptSizeBytes = ScriptSizeBytes Int64
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, ToField)

newtype NamedBudget = NamedBudget (String, Budget)
  deriving stock (Show, Generic)

instance ToNamedRecord NamedBudget where
  toNamedRecord (NamedBudget (name, Budget {..})) =
    namedRecord ["name" .= name, "cpu" .= exBudgetCPU, "mem" .= exBudgetMemory, "size" .= scriptSizeBytes]

instance DefaultOrdered NamedBudget where
  headerOrder _ = header ["name", "cpu", "mem", "size"]

mkBudget :: ExBudget -> Int64 -> Budget
mkBudget (ExBudget cpu mem) = Budget cpu mem . ScriptSizeBytes
