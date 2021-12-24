{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Criterion.Main
import Criterion.Types
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.List qualified as List
import Example.Contract.Gift.Test qualified as GiftTest
import Example.Contract.Gift.Validator.Haskell qualified as Gift
import Example.Contract.Gift.Validator.Plutarch qualified as Gift
import Example.Contract.Gift.Validator.Pluto qualified as Gift
import Pluton.Run (Budget (..))
import Pluton.Run qualified as Run
import Text.PrettyPrint.Boxes qualified as B

main :: IO ()
main = do
  exampleContractGift

exampleContractGift :: IO ()
exampleContractGift = do
  putStrLn "== Examples / Contract / Gift == "
  budgets <- execWriterT $ do
    let k = "examples:contract:gift:"
    reportBudget (k <> "haskell") $ Run.emulatorTraceBudget (GiftTest.smokeTrace Gift.haskellValidator)
    reportBudget (k <> "pluto") $ Run.emulatorTraceBudget (GiftTest.smokeTrace Gift.plutoValidator)
    reportBudget (k <> "plutarch") $ Run.emulatorTraceBudget (GiftTest.smokeTrace Gift.plutarchValidator)
  let csv = Csv.encodeDefaultOrderedByName budgets
  BSL.writeFile "bench.csv" csv
  putStrLn "Wrote to bench.csv:"
  putStrLn $ B.render $ renderNamedBudgets budgets

reportBudget :: (MonadWriter [Run.NamedBudget] m) => String -> Budget -> m ()
reportBudget name budget = do
  -- liftIO $ putStrLn $ "\t[" <> name <> "]\t\t " <> show exBudgetCPU <> " " <> show exBudgetMemory <> " " <> show scriptSizeBytes
  tell [Run.NamedBudget (name, budget)]

renderNamedBudgets :: [Run.NamedBudget] -> B.Box
renderNamedBudgets bs =
  let cols = List.transpose $ [[name, show cpu, show mem, show sz] | Run.NamedBudget (name, Run.Budget cpu mem sz) <- bs]
   in B.hsep 2 B.left . map (B.vcat B.left . map B.text) $ cols

-- TODO: remove this after https://github.com/Plutonomicon/pluton/issues/26
_placeholder :: IO ()
_placeholder = do
  defaultMainWith
    cfg
    [ bgroup
        "fib"
        [ bench "1" $ whnf fib 1,
          bench "5" $ whnf fib 5,
          bench "9" $ whnf fib 9,
          bench "11" $ whnf fib 11
        ]
    ]
  where
    cfg =
      defaultConfig
        { reportFile = Just "bench.html",
          -- The JSON/CSV reports are potentially useful for CI jobs.
          jsonFile = Just "bench.json",
          csvFile = Just "bench.csv"
        }

fib :: Int -> Int
fib m
  | m < 0 = error "negative!"
  | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n -1) + go (n -2)
