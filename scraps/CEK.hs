module CEK where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Identity

type Environment = Map.Map String Value

-- For now.
type Var = String

-- Terms
data Term = TVar Var
          | TLambda Var Term
          | TApp Term Term
          deriving (Show)

data Value = VClos Closure
          deriving (Show)

newtype Closure = Closure (Var, Term, Environment)
          deriving (Show)

data Continuation = KDone
                  | KArg Term Environment Continuation
                  | KCall Term Environment Continuation
          deriving (Show)

type CEKState = (Term, Environment, Continuation)

type CEK a = ExceptT String Identity a

step :: CEKState -> CEK CEKState
step (TVar v, e, k) =
  case Map.lookup v e of
    Just (VClos ((Closure (v', t', e')))) ->
      -- Evaluate function body in its own environment. Keep the same continuation.
      return $ ((TLambda v' t'), e', k)
    -- Just _ -> throwError "Variable in environment wasn't a closure!"
    Nothing -> throwError $ "Couldn't find " ++ v ++ " in environment!"
step ((TApp t1 t2), e, k) =
  -- Evaluate the function in the current environment, and remember to evaluate
  -- the function's argument next.
  return $ (t1, e, KArg t2 e k)
step (lam@(TLambda x t), e, (KArg t' e' k)) =
  -- Once we've evaluated a function down to a lambda expression, we need to
  -- return to evaluating the function's argument. We also need to record
  -- that we need to apply the function when we're done evaluating the argument.
  return $ (t', e', (KCall lam e k))
step (arg@(TLambda x t), e, (KCall (TLambda v tbody) e' k)) =
  -- Finally, once we've evaluated both the function and argument down to a value,
  -- we want to apply the function. To do this, we evaluate the stored function
  -- body with its variable bound to the argument we've just evaluated.
  let e'' = Map.insert v (VClos (Closure (x, t, e))) e' in
  return $ (tbody, e'', k)


isTermVal :: Term -> Bool
isTermVal (TLambda _ _) = True
isTermVal _ = False

isTerminal :: CEKState -> Bool
isTerminal (t, _, KDone) = isTermVal t
isTerminal _ = False

termToVal :: CEKState -> CEK Value
termToVal ((TLambda v t), e, k) = return $ VClos (Closure (v, t, e))
termToVal t = throwError $ "Term " ++ (show t) ++ " is not a value!"

initial :: Term -> CEKState
initial t = (t, Map.empty, KDone)

runCEK :: CEKState -> CEK Value
runCEK cek@(c, e, k)
  | isTerminal cek = termToVal cek
  | otherwise = step cek >>= runCEK


runTerm :: Term -> IO ()
runTerm t =
  case runExcept (runCEK (initial t)) of
    Left err -> putStrLn $ "Error: " ++ err
    Right v -> putStrLn $ "Result: " ++ (show v)

