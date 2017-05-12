{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving #-}
module CEKFGCBV where

import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Writer


type Environment = Map.Map String MachineValue

type Var = String

-- Terms
data Term = TReturn TermValue
          | TLet Var Term Term
          | TApp TermValue [TermValue]

deriving instance Show Term

-- We split values into two. TermValues are values which
-- appear in the source language. MachineValues are values
-- which are generated as a result of evaluating expressions.

data Builtin = Add | Sub | Mul
  deriving Show

data ValueTy = T | M
  deriving (Show)

data Value :: ValueTy -> * where
    -- Machine values
    VClos :: Closure -> Value M
    VCont :: Continuation -> Value M
    -- Term values
    VLambda :: Var -> Term -> Value T
    VVar :: Var -> Value T
    -- Both machine and term values
    VInt :: Int -> Value a
    VBuiltin :: Builtin -> Value a

deriving instance Show (Value a)

type TermValue = Value T
type MachineValue = Value M

-- A continuation frame consists of an environment, a variable, and
-- a term.
type Continuation = [ContinuationFrame]
type ContinuationFrame = (Environment, Var, Term)

-- A closure is as one would expect.
newtype Closure = Closure (Var, Term, Environment)
          deriving (Show)

type CEKState = (Term, Environment, Continuation)

-- whomst'd've
type CEK = ExceptT String (Writer [String])


interpValue :: TermValue -> Environment -> CEK MachineValue
interpValue (VVar v) e =
  case Map.lookup v e of
    Just mv -> return mv
    Nothing -> throwError $ "Could not find variable " ++ v ++ " in environment!"
interpValue (VLambda v t) e = return $ VClos (Closure (v, t, e))
interpValue (VInt i) _ = return $ VInt i
interpValue (VBuiltin b) _ = return $ VBuiltin b


showCEK :: CEKState -> String
showCEK (c, e, k) = c_str ++ e_str ++ k_str
  where c_str = "C: " ++ show c ++ "\n"
        e_str = "E: " ++ show e ++ "\n"
        k_str = "K: " ++ show k ++ "\n"


getValueIfTerminated :: CEKState -> CEK (Maybe MachineValue)
getValueIfTerminated (TReturn v, e, []) = interpValue v e >>= return . Just
getValueIfTerminated _ = return Nothing

runCEK :: CEKState -> CEK MachineValue
runCEK cek = do
  lift (tell (["Processing CEK state: " ++ (showCEK cek) ++ "\n"]))
  res <- getValueIfTerminated cek
  case res of
    Just v -> return v
    Nothing -> step cek >>= runCEK

--      mapM (flip $ interpValue e) vArgs >>= \mvArgs ->

evalBuiltin :: Builtin -> [TermValue] -> Environment -> CEK TermValue
evalBuiltin op [tv1, tv2] e = do
    (VInt i1) <- interpValue tv1 e
    (VInt i2) <- interpValue tv2 e
    return . VInt $ (interpOp op) i1 i2
  where
    interpOp Add = (+)
    interpOp Mul = (*)
    interpOp Sub = (-)

step :: CEKState -> CEK CEKState
step ((TApp v1 vArgs), e, k) =
  -- Interpret the lambda abstraction
  interpValue v1 e >>= \mv1 ->
  case (mv1, vArgs) of
    -- M-App: extend environment with interpreted argument, evaluate
    -- function body.
    (VClos (Closure (x, t, e')), [mv2]) ->
      -- Interpret the argument list
      interpValue mv2 e >>= \mv2 ->
      -- Extend the captured value with the interpretation of v2
      let e'' = (Map.insert x mv2 e') in
      -- Evaluate the function body wrt the new env.
      return (t, e'', k)
    -- M-AppCont: if function is captured continuation, return argument,
    -- then add continuation onto continuation stack.
    (VCont k', [v2]) -> return (TReturn v2, e, k' ++ k)
    (VBuiltin b, vs) ->
      evalBuiltin b vs e >>= \res ->
      return (TReturn res, e, k)
step ((TLet x t1 t2), e, k) =
  -- Evaluate t1, push the continuation onto the stack.
  return (t1, e, (e, x, t2) : k)
-- M-RetCont: given a returned value and a continuation frome, evaluates
-- the captured continuation frame with the environment extended with the
-- value.
step ((TReturn tv), e, (e', x, t) : k) =
  -- Interpret term value
  interpValue tv e >>= \mv ->
  -- Extend env
  let e'' = Map.insert x mv e' in
  -- Update machine state
  return (t, e'', k)
step cek = throwError $ "Invalid CEK state! " ++ showCEK cek


termIdentApp :: Term
termIdentApp =
  let lam1 = VLambda "x" (TReturn (VVar "x")) in
  let lam2 = VLambda "y" (TReturn (VVar "y")) in
  TApp lam1 [lam2]

termIdentApp2 :: Term
termIdentApp2 =
  TLet "x" (TReturn $ VLambda "x" (TReturn (VVar "x")))
    (TApp (VVar "x") [(VLambda "y" (TReturn (VVar "y")))])


termAdd :: Term
termAdd =
  TLet "x" (TReturn $ VLambda "x" (TReturn (VVar "x")))
    (TApp (VBuiltin Add) [VInt 1, VInt 2])

initial :: Term -> CEKState
initial t = (t, Map.empty, [])

printLog :: [String] -> IO ()
printLog xs = putStrLn "Trace: " >> mapM_ putStrLn xs

runTerm :: Term -> IO ()
runTerm t =
  let m_writer = runExceptT (runCEK (initial t)) in
  let (res, output) = runWriter m_writer in
  printLog output >>
  case res of
    Left err -> putStrLn $ "Error: " ++ err
    Right v -> putStrLn $ "Result: " ++ (show v)

