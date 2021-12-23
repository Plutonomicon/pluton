{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Type-safe builtins.
--
-- To define the type of a particular builtin, write a type instance for
-- `PBuiltinFunType`. This enables `pBuiltinFun` to be used on that PLC builtin:
--
-- @(pBuiltinFun @'PLC.UnIData @'[] # someData)@
module Pluton.Types.Builtin
  ( module X,
  )
where

import Pluton.Types.Builtin.Fun as X
import Pluton.Types.Builtin.Trace as X
