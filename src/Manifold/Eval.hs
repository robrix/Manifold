{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Manifold.Eval where

import Control.Applicative
import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semilattice.Lower
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pattern
import Manifold.Proof
import Manifold.Term as Term
import Manifold.Term.Elim
import Manifold.Term.Intro
import Manifold.Value as Value

eval :: Member (Reader Environment) effects
     => Term Name
     -> Proof usage effects Value
eval (Term term) = case term of
  Var name -> fmap constraintValue . contextLookup name <$> askEnv >>= maybe (error "free variable, should have been caught by typechecker") pure
  Intro i -> case i of
    Abs var body -> do
      env <- contextFilter (((&&) <$> (/= name var) <*> (`elem` freeVariables body)) . name) <$> ask
      pure (Value (Abs (name var ::: env) body))
    Data c as -> Value . Data c <$> traverse eval as
  Elim e -> case e of
    App f a -> do
      v <- eval f
      case unValue v of
        Abs (name ::: env) body -> do
          -- FIXME: use the env
          -- FIXME: pi types
          a' <- eval a
          env `seq` name .= a' $ eval body
        _ -> error "application of non-abstraction, should have been caught by typechecker"
    If c t e -> do
      v <- eval c
      case unValue v of
        Data (N "True")  [] -> eval t
        Data (N "False") [] -> eval e
        _ -> error "branch on non-boolean, should have been caught by typechecker"
    Case s bs -> do
      s' <- eval s
      case foldr (\ (pattern, branch) rest -> flip (,) branch <$> match s' pattern <|> rest) Nothing bs of
        Just (f, a) -> f (eval a)
        _ -> error "non-exhaustive pattern match, should have been caught by typechecker"

askEnv :: Member (Reader Environment) effects => Proof usage effects Environment
askEnv = ask


runEnv :: Effects effects => Proof usage (Reader Environment ': effects) a -> Proof usage effects a
runEnv = runReader lowerBound


(.=) :: Member (Reader Environment) effects => Name -> Value -> Proof usage effects a -> Proof usage effects a
name .= value = local (|> (name ::: value))

infixl 1 .=


match :: Member (Reader Environment) effects => Value -> Pattern Name -> Maybe (Proof usage effects a -> Proof usage effects a)
match _ (Pattern Wildcard) = Just id
match s (Pattern (Variable name)) = Just (name .= s)
match (Value.Value (Data c vs)) (Pattern (Constructor c' ps))
  | c == c', length vs == length ps = foldr (.) id <$> sequenceA (zipWith match vs ps)
match _ _ = Nothing
