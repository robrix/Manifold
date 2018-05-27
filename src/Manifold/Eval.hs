{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Manifold.Eval where

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Manifold.Constraint
import Manifold.Context
import Manifold.Expr
import Manifold.Name
import Manifold.Proof
import Manifold.Term
import Manifold.Value

eval :: Member (Reader (Context Name Value)) effects
     => Term
     -> Proof usage effects Value
eval (Term term) = case term of
  Var name -> fmap constraintValue . contextLookup name <$> askEnv >>= maybe (error "free variable, should have been caught by typechecker") pure
  Intro i -> case i of
    Unit -> pure (Value Unit)
    Bool b -> pure (Value (Bool b))
    Abs (var ::: _) body -> do
      env <- contextFilter (((&&) <$> (/= name var) <*> (`elem` freeVariables body)) . constraintName) <$> ask
      pure (Value (Abs (name var ::: env) body))
    Pair a b -> fmap Value . Pair <$> eval a <*> eval b
  IntroT _ -> error "types are unrepresentable at runtime"
  Elim e -> case e of
    ExL pair -> eval pair >>= \ p -> case p of { Value (Pair a _) -> pure a ; _ -> error "exl on non-pair value, should have been caught by typechecker" }
    ExR pair -> eval pair >>= \ p -> case p of { Value (Pair _ b) -> pure b ; _ -> error "exr on non-pair value, should have been caught by typechecker" }
    App f a -> do
      v <- eval f
      case v of
        Value (Abs (name ::: env) body) -> do
          -- FIXME: use the env
          -- FIXME: pi types
          a' <- eval a
          env `seq` name ::: a' >- eval body
        _ -> error "application of non-abstraction, should have been caught by typechecker"
    If c t e -> do
      v <- eval c
      case v of
        Value (Bool b) ->
          if b then
            eval t
          else
            eval e
        _ -> error "branch on non-boolean, should have been caught by typechecker"

askEnv :: Member (Reader (Context Name Value)) effects => Proof usage effects (Context Name Value)
askEnv = ask


runEnv :: Proof usage (Reader (Context Name Value) ': effects) a -> Proof usage effects a
runEnv = runReader emptyContext
