module CEK2 where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Identity
import Debug.Trace

type Environment = Map.Map String Value

-- For now.
type Var = String

-- Terms
data Term = TStop -- that weird dagger-y thing
          | TInt Int
          | TVar Var
          | TLambda Var Term
          | TAdd Term Term
          | TApp Term Term
          deriving (Show)

data Value = VClos Closure
           | VInt Int
          deriving (Show)

newtype Closure = Closure (Var, Term, Environment)
          deriving (Show)


type Continuation = [ContinuationFrame]

data ContinuationFrame =
    KArg Term Environment
  | KFun Value
  | KAdd Term
  | KAddRes Value
  | KRet Value
  deriving (Show)

type CEKState = (Term, Environment, Continuation)

type CEK a = ExceptT String Identity a


showCEK :: CEKState -> String
showCEK (c, e, k) = c_str ++ e_str ++ k_str
  where c_str = "C: " ++ show c ++ "\n"
        e_str = "E: " ++ show e ++ "\n"
        k_str = "K: " ++ show k ++ "\n"

step :: CEKState -> CEK CEKState
step (TVar v, env, k) =
  case Map.lookup v env of
    Just v -> return $ (TStop, Map.empty, (KRet v) :  k)
    Nothing -> throwError $ "Couldn't find " ++ v ++ " in environment!"
step (TLambda v t, env, k) =
  -- Capture environment, karg
  let clos = Closure (v, t, env) in
  return $ (TStop, Map.empty, KRet (VClos clos) : k)
step (TApp t1 t2, env, k) =
  -- Evaluate t1 under current env, remember to eval arg later
  return $ (t1, env, (KArg t2 env) : k)
step (TStop, _, (KRet f_val) : (KArg t2 e ) : k) =
  -- Evaluate t2 with stored environment, remember to call later
  return $ (t2, e, (KFun f_val) : k)
step cek@(TStop, _, (KRet v_arg) : (KFun v_clos) : k) =
  -- We've evaluated t1 to a lambda. Now we need to evaluate the argument.
  -- We've evaluated the value to the function, and now we want
  -- to evaluate the function body with the captured env extended with
  -- the new value.
  let (VClos clos) = v_clos in
  let (Closure (x, tbody, clos_env)) = clos in
  let clos_env' = Map.insert x v_arg clos_env in
  return $ (tbody, clos_env', k)

-- Base operations (integers)
step (TInt i, env, k) = return $ (TStop, Map.empty, (KRet (VInt i)) : k)
step (TAdd t1 t2, env, k) = return $ (t1, env, KAdd t2 : k)
step (TStop, env, (KRet v1) : (KAdd t2) : k) = return $ (t2, env, (KAddRes v1) : k)
step (TStop, env, (KRet (VInt i1)) : (KAddRes (VInt i2)) : k) = return $ (TStop, env, KRet (VInt (i1 + i2)) : k)
step cek = throwError $ "Invalid CEK state : " ++ (showCEK cek)


getValueIfTerminated :: CEKState -> Maybe Value
getValueIfTerminated (TStop, _, [KRet v]) = Just v
getValueIfTerminated _ = Nothing

initial :: Term -> CEKState
initial t = (t, Map.empty, [])

runCEK :: CEKState -> CEK Value
runCEK cek =
  trace ("Processing CEK state: " ++ (showCEK cek) ++ "\n") $
  case getValueIfTerminated cek of
    Just v -> return v
    Nothing -> step cek >>= runCEK


testTerm1 :: Term
testTerm1 = TAdd (TAdd (TInt 100) (TInt 200)) (TApp (TLambda "x" (TVar "x")) (TInt 300))

runTerm :: Term -> IO ()
runTerm t =
  case runExcept (runCEK (initial t)) of
    Left err -> putStrLn $ "Error: " ++ err
    Right v -> putStrLn $ "Result: " ++ (show v)

