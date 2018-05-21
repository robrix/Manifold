{-# LANGUAGE FlexibleContexts #-}
module Manifold.Eval where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semiring (zero)
import Manifold.Context
import Manifold.Expr hiding (Bool, BoolT, Pair, TypeT, Unit, UnitT)
import qualified Manifold.Expr as Expr
import Manifold.Proof
import Manifold.Value

eval :: ( Member (Reader (Context usage (Value usage))) effects
        , Monoid usage
        )
     => Term usage
     -> Proof usage effects (Value usage)
eval (Term term) = case term of
  -- FIXME: no failable patterns
  Var name -> do
    Just value <- fmap constraintValue . contextLookup name <$> askEnv
    pure value
  Intro i -> case i of
    Expr.Unit -> pure Unit
    Expr.Bool b -> pure (Bool b)
    Abs ((name, _) ::: _) body -> Closure name body . contextFilter (((&&) <$> (/= name) <*> (`elem` freeVariables body)) . constraintName) <$> ask
    Expr.Pair a b -> Pair <$> eval a <*> eval b
    Expr.UnitT -> pure UnitT
    Expr.BoolT -> pure BoolT
    Expr.TypeT -> pure TypeT
    (name, _) ::: _ :-> body -> Closure name body . contextFilter (((&&) <$> (/= name) <*> (`elem` freeVariables body)) . constraintName) <$> ask
    a :* b -> Product <$> eval a <*> eval b
  Elim e -> case e of
    ExL pair -> do
      Pair a _ <- eval pair
      pure a
    ExR pair -> do
      Pair _ b <- eval pair
      pure b
    App f a -> do
      Closure name body env <- eval f
      -- FIXME: use the env
      a' <- eval a
      (name, zero) ::: a' >- eval body
    If c t e -> do
      Bool b <- eval c
      if b then
        eval t
      else
        eval e

askEnv :: Member (Reader (Context usage (Value usage))) effects => Proof usage effects (Context usage (Value usage))
askEnv = ask
