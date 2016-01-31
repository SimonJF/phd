{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module LCh where

import Bound
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans
import qualified Data.Map as M

-- | LCh Types
data LChTy = LChOneTy
           | LChPairTy LChTy LChTy
           | LChSumTy LChTy LChTy
           | LChFunTy LChTy LChTy
           | LChListTy LChTy
           | LChChanTy LChTy
           | LChTyVar String
  deriving (Eq)

-- | LCh Terms
data LChTerm a = LChVar a
               -- \x.M
               | LChLam (LChTerm a) LChTy (Scope () LChTerm a)
               -- (M, N)
               | LChPair (LChTerm a) (LChTerm a)
               -- let (x, y) = M in N
               | LChLetPair (LChTerm a) (LChTerm a) (Scope Int LChTerm a)
               -- rec f(x: A): B . M
               | LChRec (Scope Int (LChTerm a)) LChTy LChTy (Scope Int (LChTerm a))
               -- inl M
               | LChInl (LChTerm a)
               -- inr M
               | LChInr (LChTerm a)
               -- case M { inl x |-> N; inr y |-> N'}
               | LChPairCase (LChTerm a) (Scope () LChTerm a) (Scope () LChTerm a)
               -- []
               | LChEmptyList
               -- [M]
               | LChSingletonList (LChTerm a)
               -- M ++ N
               | LChAppend (LChTerm a) (LChTerm a)
               -- case L ([] |-> N; [M] ++ M' |-> N'}
               | LChListCase (LChTerm a) (LChTerm a) (Scope Int LChTerm a)
               -- ()
               | LChUnit
               -- M ; N
               | LChLetUnit (LChTerm a) (LChTerm a)
               -- fork M
               | LChFork (LChTerm a)
               -- give M N
               | LChGive (LChTerm a) (LChTerm a)
               -- take M
               | LChTake (LChTerm a)
               -- newCh
               | LChNewCh
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- Typechecker type
type TcM a = ErrorT String (Gen a)
type Env a = M.Map String a

assertTy :: Ord a => Env a -> LChTerm -> LChTy -> TcM ()
assertTy e t ty = (== ty) <$> (typeCheck e t) >>= guard

typeCheck :: Ord a => Env a -> LChTerm -> TcM LChTy
typeCheck e (LChVar x) = undefined
typeCheck e (LChLam lterm lty scope) = undefined
typeCheck e (LChPair t1 t2) = undefined
typeCheck e (LChLetPair t1 t2 s1) = undefined
typeCheck e (LChRec s1 argTy funTy s2) = undefined
typeCheck e (LChInl t) = undefined
typeCheck e (LChInr t) = undefined
typeCheck e (LChPairCase scrutinee s1 s2) = undefined
typeCheck e (LChEmptyList) = undefined
typeCheck e (LChSingletonList t) =
  LChListTy <$> typeCheck e t
typeCheck e (LChAppend t1 t2) = undefined
typeCheck e (LChListCase scrutinee nilTerm appendScope) = undefined
typeCheck e (LChUnit) = LChOneTy
typeCheck e (LChLetUnit t1 t2) = undefined
typeCheck e (LChFork t) = undefined
typeCheck e (LChGive t1 t2) = undefined
typeCheck e (LChTake t) =
  do chanTy <- typeCheck e t
     case chanTy of
       (LChChanTy ty) -> ty
       badTy ->
         throwError $ "Attempting to take from non-channel type " ++ badTy
typeCheck e (LChNewCh) = LChChanTy



-- Instances
instance Monad Exp where
  return = LChVar
  (LChVar a) >>= f = f a
  (LChLam t1 _ s1) >>= f = (LChLam (t1 >>= f) (s1 >>>= f))
  (LChPair t1 t2) >>= f = (LChPair (t1 >>= f) (t2 >>= f))
  (LChLetPair t1 t2 body) >>= f =
    (LChLetPair (t1 >>= f) (t2 >>= f) (body >>>= f))
  (LChRec funScope _ _ bodyScope) >>= f =
    (LChRec (funScope >>>= f) (bodyScope >>>= f) )
  (LChInl t) f = (LChInl (t >>= f))
  (LChInr t) f = (LChInr (t >>= f))
  (LChPairCase scrutinee s1 s2) =
    (LChPairCase (scrutinee >>= f) (s1 >>>= f) (s2 >>>= f))
  LChEmptyList _ = LChEmptyList
  (LChSingletonList t) f = LChSingletonList (t >>= f)
  (LChAppend t1 t2) f = (LChAppend (t1 >>= f) (t2 >>= f))
  (LChListCase t1 t2 scope) =
    (LChListCase (t1 >>= f) (t2 >>= f) (scope >>>= f))
  LChUnit _ = LChUnit
  (LChLetUnit t1 t2) f = (LChLetUnit (t1 >>= f) (t2 >>= f))
  (LChFork t) f = (LChFork (t >>= f))
  (LChGive t1 t2) f = (LChGive (t1 >>= f) (t2 >>= f))
  (LChTake t1) f = (LChTake (t1 >>= f))
  LChNewCh _ = LChNewCh
