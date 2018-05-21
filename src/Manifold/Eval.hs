{-# LANGUAGE FlexibleContexts #-}
module Manifold.Eval where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semiring (zero)
import Manifold.Constraint
import Manifold.Context
import Manifold.Expr
import Manifold.Proof
import Manifold.Term
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
    Unit -> pure (Value Unit)
    Bool b -> pure (Value (Bool b))
    Abs ((name, _) ::: _) body -> do
      env <- contextFilter (((&&) <$> (/= name) <*> (`elem` freeVariables body)) . constraintName) <$> ask
      pure (Value (Abs (name, env) body))
    Pair a b -> fmap Value . Pair <$> eval a <*> eval b
    UnitT -> pure (Value UnitT)
    BoolT -> pure (Value BoolT)
    TypeT -> pure (Value TypeT)
    (name, _) ::: _ :-> body -> do
      env <- contextFilter (((&&) <$> (/= name) <*> (`elem` freeVariables body)) . constraintName) <$> ask
      pure (Value ((name, env) :-> body))
    a :* b -> fmap Value . (:*) <$> eval a <*> eval b
  Elim e -> case e of
    ExL pair -> do
      Value (Pair a _) <- eval pair
      pure a
    ExR pair -> do
      Value (Pair _ b) <- eval pair
      pure b
    App f a -> do
      Value (Abs (name, env) body) <- eval f
      -- FIXME: use the env
      -- FIXME: pi types
      a' <- eval a
      (name, zero) ::: a' >- eval body
    If c t e -> do
      Value (Bool b) <- eval c
      if b then
        eval t
      else
        eval e

askEnv :: Member (Reader (Context usage (Value usage))) effects => Proof usage effects (Context usage (Value usage))
askEnv = ask
