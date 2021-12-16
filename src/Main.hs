{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Main (main, tests) where

import Control.Lens.Combinators (makeLenses, view)
import Control.Lens.Operators ((^.))
import Control.Monad (void)
import Control.Monad.Freer.Extras qualified as Extras
import Data.Text (Text)
import Ledger hiding (singleton)
import Ledger.Ada as Ada (lovelaceValueOf)
import Plut.Sample.Offchain
import Plut.Sample.Validator.Haskell (haskellValidator)
import Plut.Sample.Validator.Pluto (plutoValidator)
import Plutus.Contract.Test (w1, w2, w3)
import Plutus.Contract.Test qualified as PCT
import Plutus.Contract.Test.ContractModel qualified as PCT
import Plutus.Trace.Emulator (EmulatorTrace)
import Plutus.Trace.Emulator qualified as Em
import Test.QuickCheck.Gen qualified as QC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as H
import Test.Tasty.QuickCheck qualified as QC
import Wallet.Emulator.Wallet (Wallet, knownWallets)

-- -------------------------------------------------------------------------- --
-- Tests                                                                      --
-- -------------------------------------------------------------------------- --

data SampleModel = SampleModel
  { _giftAmount :: Integer
  , _giftBeneficiary :: Maybe Wallet
  }
  deriving stock (Prelude.Eq, Prelude.Show)

makeLenses ''SampleModel

genWallet :: QC.Gen Wallet
genWallet = QC.elements knownWallets

instance PCT.ContractModel SampleModel where
  data Action SampleModel = Give Wallet Wallet Integer | Grab Wallet
    deriving stock (Prelude.Show, Prelude.Eq)

  data ContractInstanceKey SampleModel _ _ _ where
    UseContract :: Wallet -> PCT.ContractInstanceKey SampleModel () GiftSchema Text

  arbitraryAction :: PCT.ModelState SampleModel -> QC.Gen (PCT.Action SampleModel)
  arbitraryAction _ = do
    wallet <- genWallet
    beneficiary <- genWallet
    let minAda = 2
        val = (1000000 *) Prelude.<$> QC.choose @Integer (minAda, 5)
    QC.oneof
      [ Prelude.fmap (Give wallet beneficiary) val
      , Prelude.pure $ Grab wallet
      ]

  initialState :: SampleModel
  initialState = SampleModel 0 Nothing

  precondition :: PCT.ModelState SampleModel -> PCT.Action SampleModel -> Bool
  precondition s (Give _w giftee _) =
    s ^. PCT.contractState . giftBeneficiary Prelude.== Just giftee
  precondition s (Grab _w) =
    s ^. PCT.contractState . giftAmount Prelude.>= 0

  nextState :: PCT.Action SampleModel -> PCT.Spec SampleModel ()
  nextState = \case
    Give w giftee v -> do
      PCT.withdraw w (lovelaceValueOf v)
      giftAmount PCT.$~ (Prelude.+ v)
      giftBeneficiary PCT.$= Just giftee
      PCT.wait 1
    Grab w -> do
      v <- PCT.askContractState (view giftAmount)
      PCT.deposit w (lovelaceValueOf v)
      giftAmount PCT.$= 0
      PCT.wait 1

  perform :: PCT.HandleFun SampleModel -> PCT.ModelState SampleModel -> PCT.Action SampleModel -> EmulatorTrace ()
  perform h _ = \case
    Give w giftee n -> do
      let pkh = PCT.walletPubKeyHash giftee
      Em.callEndpoint @"give" (h $ UseContract w) (pkh, n)
      void $ Em.waitNSlots 1
    Grab w -> do
      Em.callEndpoint @"grab" (h $ UseContract w) ()
      void $ Em.waitNSlots 1

deriving stock instance Prelude.Show (PCT.ContractInstanceKey SampleModel w s e)

deriving stock instance Prelude.Eq (PCT.ContractInstanceKey SampleModel w s e)

instanceSpec :: Validator -> [PCT.ContractInstanceSpec SampleModel]
instanceSpec vl = [PCT.ContractInstanceSpec (UseContract w) w (endpoints vl) | w <- knownWallets]

modelCheck :: Validator -> PCT.Actions SampleModel -> QC.Property
modelCheck vl =
  QC.withMaxSuccess 10
    . PCT.propRunActionsWithOptions
      PCT.defaultCheckOptions
      (instanceSpec vl)
      (Prelude.const $ Prelude.pure True)

noFundsLocked :: Validator -> QC.Property
noFundsLocked vl =
  QC.withMaxSuccess 10 $
    PCT.checkNoLockedFundsProof
      PCT.defaultCheckOptions
      (instanceSpec vl)
      (PCT.NoLockedFundsProof (PCT.action $ Grab w1) (PCT.action . Grab))

smokeTrace :: Validator -> EmulatorTrace ()
smokeTrace validator = do
  let ep = endpoints validator
  h1 <- Em.activateContractWallet w1 ep
  h2 <- Em.activateContractWallet w2 ep
  h3 <- Em.activateContractWallet w3 ep
  Extras.logInfo $ "Beneficiary: " <> show w3
  Em.callEndpoint @"give" h1 (PCT.walletPubKeyHash w3, 10 * 1000000)
  void $ Em.waitUntilSlot 1
  Em.callEndpoint @"grab" h2 () -- Not a benefiary
  _s <- Em.waitNSlots 1
  Em.callEndpoint @"grab" h3 ()
  _s <- Em.waitNSlots 1
  Extras.logInfo "done"

tests :: IO ()
tests = do
  T.defaultMain $
    T.localOption (H.HedgehogTestLimit (Just 10)) . T.localOption (H.HedgehogShrinkLimit (Just 2)) $
      T.testGroup "Sample Contract" $
        flip fmap [("Haskell", haskellValidator), ("Pluto", plutoValidator)] $ \(k, validator) ->
          T.testGroup
            ("Validator:" <> k)
            [ QC.testProperty "contract" (modelCheck validator)
            , QC.testProperty "nofundslocked" (noFundsLocked validator)
            ]

main :: IO ()
main = do
  Em.runEmulatorTraceIO $ smokeTrace plutoValidator
  tests
