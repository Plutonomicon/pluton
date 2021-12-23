module Pluton.Evaluate
  ( -- * General evaluation of UPLC
    eval,
    evalWithArgs,

    -- * Evaluation of Plutarch eDSL
    evalPlutarch,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Plutarch qualified
import Plutus.V1.Ledger.Scripts (Script)
import Plutus.V1.Ledger.Scripts qualified as Scripts
import PlutusCore.Data qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusTx.Evaluation (evaluateCekTrace)
import UntypedPlutusCore
  ( DefaultFun,
    DefaultUni,
    Name,
    Term,
  )
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as UPLC

data Error = Error
  { errorTraces :: [Text],
    -- | Trace log of the script
    errorPlutus :: Scripts.ScriptError
  }
  deriving stock (Show)

evalPlutarch :: HasCallStack => Plutarch.ClosedTerm a -> (ExBudget, [Text], Term Name DefaultUni DefaultFun ())
evalPlutarch p =
  case eval (Plutarch.compile p) of
    Left err -> error (show err)
    Right res -> res

eval :: Script -> Either Error (ExBudget, [Text], Term Name DefaultUni DefaultFun ())
eval = evaluateScript @(Either Error)

evalWithArgs :: [PLC.Data] -> Script -> Either Error (ExBudget, [Text], Term Name DefaultUni DefaultFun ())
evalWithArgs args =
  evaluateScript @(Either Error)
    . flip Scripts.applyArguments args

-- | Evaluate a script, returning the trace log and term result.
--
-- This is same as `Plutus.V1.Ledger.Scripts.evaluateScript`, but returns the
-- script result as well.
evaluateScript ::
  forall m uni fun.
  (MonadError Error m, uni ~ DefaultUni, fun ~ DefaultFun) =>
  Script ->
  m (ExBudget, [Text], Term Name uni fun ())
evaluateScript s = do
  p <- case Scripts.mkTermToEvaluate s of
    Right p -> pure p
    Left e -> throwError $ Error mempty $ Scripts.MalformedScript $ show e
  let (logOut, UPLC.TallyingSt _ budget, result) = evaluateCekTrace p
  term <- case result of
    Right term -> pure term
    Left errWithCause@(UPLC.ErrorWithCause err cause) ->
      throwError $
        Error logOut $ case err of
          UPLC.InternalEvaluationError internalEvalError ->
            Scripts.EvaluationException (show errWithCause) (show internalEvalError)
          UPLC.UserEvaluationError evalError ->
            -- We use `show` here because plutus doesn't expose mkError
            Scripts.EvaluationError logOut (show (evalError, cause))
  pure (budget, logOut, term)
