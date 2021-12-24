module Main (main) where

import Criterion.Main
import Criterion.Types
import Example.Contract.Gift.Test qualified as GiftTest
import Example.Contract.Gift.Validator.Haskell qualified as Gift
import Example.Contract.Gift.Validator.Plutarch qualified as Gift
import Example.Contract.Gift.Validator.Pluto qualified as Gift
import Pluton.Run qualified as Run

main :: IO ()
main = do
  exampleContractGift
  placeholder

exampleContractGift :: IO ()
exampleContractGift = do
  putStrLn "\n== Sample contract - Gift: sizes (haskell; pluto; plutarch) == "
  print $ Run.emulatorTraceBudget (GiftTest.smokeTrace Gift.haskellValidator)
  print $ Run.emulatorTraceBudget (GiftTest.smokeTrace Gift.plutoValidator)
  print $ Run.emulatorTraceBudget (GiftTest.smokeTrace Gift.plutarchValidator)


-- TODO: remove this after https://github.com/Plutonomicon/pluton/issues/26
placeholder :: IO ()
placeholder = do
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
