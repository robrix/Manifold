{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Manifold.Type.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.State
import Data.Functor (($>))
import Data.Semiring (Semiring(..), zero)
import Manifold.Binding
import Manifold.Constraint
import Manifold.Context
import Manifold.Expr
import Manifold.Name
import Manifold.Proof
import Manifold.Substitution
import Manifold.Term
import Manifold.Type
import Manifold.Type.Formation
import Manifold.Unification

check :: ( Eq usage
         , Members '[ Exc (Error usage)
                    , Fresh
                    , Reader (Context usage (Type usage))
                    , State (Substitution (Type usage))
                    ] effects
         , Monoid usage
         , Semiring usage
         )
      => Term usage
      -> Type usage
      -> Proof usage effects (Type usage)
check term expected = do
  actual <- infer term
  unify actual expected

infer :: ( Eq usage
         , Members '[ Exc (Error usage)
                    , Fresh
                    , Reader (Context usage (Type usage))
                    , State (Substitution (Type usage))
                    ] effects
         , Monoid usage
         , Semiring usage
         )
      => Term usage
      -> Proof usage effects (Type usage)
infer term = case unTerm term of
  Var name                                 -> do
    context <- askContext
    maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  Intro i
    | Unit                  <- i -> pure unitT
    | Bool _                <- i -> pure boolT
    | Abs (var ::: ty) body <- i -> do
      ty' <- checkIsType ty
      body' <- var ::: ty' >- infer body
      pure (var ::: ty' .-> body')
    | Pair a b              <- i -> (.*) <$> infer a <*> infer b
    | UnitT                 <- i -> pure typeT
    | BoolT                 <- i -> pure typeT
    | TypeT                 <- i -> pure typeT
    | var ::: ty :-> body   <- i -> do
      ty' <- checkIsType ty
      (var ::: ty' >- checkIsType body) $> typeT
    | a :* b                <- i -> checkIsType a *> checkIsType b $> typeT
  Elim e
    | ExL a    <- e -> do
      t1 <- tvar . I <$> fresh
      t2 <- tvar . I <$> fresh
      _ <- check a (t1 .* t2)
      pure t1
    | ExR a    <- e -> do
      t1 <- tvar . I <$> fresh
      t2 <- tvar . I <$> fresh
      _ <- check a (t1 .* t2)
      pure t2
    | App f a  <- e -> do
      n <- I <$> fresh
      t1 <- tvar . I <$> fresh
      t2 <- tvar . I <$> fresh
      _ <- check f (Binding n zero ::: t1 .-> t2)
      _ <- check a t1
      pure t2
    | If c t e <- e -> do
      _ <- check c boolT
      t' <- infer t
      e' <- infer e
      unify t' e'


runSubstitution :: Proof usage (State (Substitution (Type usage)) ': effects) a -> Proof usage effects (a, Substitution (Type usage))
runSubstitution = runState mempty
