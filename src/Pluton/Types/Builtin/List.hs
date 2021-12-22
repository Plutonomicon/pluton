{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pluton.Types.Builtin.List
  ( -- * List type
    PList (..),
    ListElemUni (..),

    -- * List construction
    cons,
    nil,
    mkList,
    singleton,
    append,

    -- * List access
    head,
    contains,
    atIndex,
  )
where

import Data.ByteString qualified as BS
import Data.Proxy
import Data.Text (Text)
import Data.Type.Nat
import Plutarch
import Plutarch.Bool
import Plutarch.ByteString
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.String
import Plutarch.Unit
import Pluton.Types.Builtin
import Pluton.Types.Builtin qualified as B
import PlutusCore qualified as PLC
import PlutusCore.Data qualified as PLC
import Prelude hiding (head)

data PList a s
  = PNil
  | PCons (Term s a) (Term s (PList a))

class ListElemUni (a :: k -> Type) where
  type ListElemType a :: Type
  listElemUni :: Proxy a -> PLC.DefaultUni (PLC.Esc (ListElemType a))

instance ListElemUni POpaque where
  type ListElemType POpaque = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData

instance ListElemUni PInteger where
  type ListElemType PInteger = Integer
  listElemUni Proxy = PLC.DefaultUniInteger

instance ListElemUni PString where
  type ListElemType PString = Text
  listElemUni Proxy = PLC.DefaultUniString

instance ListElemUni PByteString where
  type ListElemType PByteString = BS.ByteString
  listElemUni Proxy = PLC.DefaultUniByteString

instance ListElemUni PUnit where
  type ListElemType PUnit = ()
  listElemUni Proxy = PLC.DefaultUniUnit

instance ListElemUni PBool where
  type ListElemType PBool = Bool
  listElemUni Proxy = PLC.DefaultUniBool

instance ListElemUni (a :: k -> Type) => PlutusType (PList a) where
  type PInner (PList a) _ = PList a
  pcon' PNil =
    punsafeConstant $
      PLC.Some $
        PLC.ValueOf (PLC.DefaultUniList $ listElemUni (Proxy :: Proxy a)) []
  pcon' (PCons x xs) = B.ppBuiltin @'PLC.MkCons @'[a] £ x £ xs
  pmatch' = pmatchList

type instance PPBuiltinType 'PLC.MkCons '[a] = a :--> PList a :--> PList a

type instance PPBuiltinForce 'PLC.MkCons = Nat0

type instance PPBuiltinType 'PLC.NullList '[a] = PList a :--> PBool

type instance PPBuiltinForce 'PLC.NullList = Nat1

type instance PPBuiltinType 'PLC.HeadList '[a] = PList a :--> a

type instance PPBuiltinForce 'PLC.HeadList = Nat1

type instance PPBuiltinType 'PLC.TailList '[a] = PList a :--> PList a

type instance PPBuiltinForce 'PLC.TailList = Nat1

pmatchList :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). Term s (PList a) -> (PList a s -> Term s b) -> Term s b
pmatchList list f =
  plet (B.ppBuiltin @'PLC.NullList @'[a] £ list) $ \isEmpty ->
    pif
      (punsafeCoerce isEmpty)
      (f PNil)
      $ plet
        (B.ppBuiltin @'PLC.HeadList @'[a] £ list)
        ( \head ->
            plet (B.ppBuiltin @'PLC.TailList @'[a] £ list) $ \tail ->
              f $ PCons head tail
        )

cons :: forall k (s :: k) (a :: k -> Type). ListElemUni a => Term s a -> Term s (PList a) -> Term s (PList a)
cons x xs = pcon' $ PCons x xs

nil :: forall k (s :: k) (a :: k -> Type). ListElemUni a => Term s (PList a)
nil = pcon' PNil

-- | Build a polymorphic list from a list of PLC terms.
mkList :: forall k (s :: k) (a :: k -> Type). ListElemUni a => [Term s a] -> Term s (PList a)
mkList = \case
  [] -> nil
  (x : xs) -> cons x (mkList xs)

-- | Return the head of a list; fails on empty list.
head :: forall k (s :: k) (c :: k -> Type). Term s (PList c) -> Term s c
head list =
  pmatchList list $ \case
    PNil -> perror
    PCons x _ -> x

-- | Create a singleton list
singleton :: ListElemUni a => Term s (a :--> PList a)
singleton =
  plam $ \x ->
    pcon' (PCons x $ pcon' PNil)

-- | Return True if the list contains the given element.
contains :: (PEq a, ListElemUni a) => ClosedTerm (PList a :--> a :--> PBool)
contains =
  pfix £$ plam $ \self list k ->
    pmatch' list $ \case
      PNil ->
        pcon PFalse
      PCons x xs ->
        pif
          (k £== x)
          (pcon PTrue)
          (self £ xs £ k)

-- | Return the element at given index; fail otherwise.
atIndex :: ListElemUni a => ClosedTerm (PInteger :--> PList a :--> a)
atIndex =
  pfix £$ plam $ \self n' list ->
    pmatch' list $ \case
      PNil ->
        perror
      PCons x xs ->
        pif
          (n' £== 0)
          x
          (self £ (n' - 1) £ xs)

-- | Append two lists
append :: ListElemUni a => ClosedTerm (PList a :--> PList a :--> PList a)
append =
  pfix £$ plam $ \self list1 list2 ->
    pmatch' list1 $ \case
      PNil ->
        list2
      PCons x xs ->
        pcon' (PCons x $ self £ xs £ list2)
