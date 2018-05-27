{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Manifold.Type.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Functor (($>))
import Data.Semiring (zero)
import Manifold.Name.Annotated
import Manifold.Constraint
import Manifold.Context
import Manifold.Expr
import Manifold.Name
import Manifold.Proof
import Manifold.Purpose
import Manifold.Substitution
import Manifold.Term
import Manifold.Type
import Manifold.Type.Formation
import Manifold.Unification

check :: ( Eq usage
         , Members '[ Exc (Error (Binding usage))
                    , Fresh
                    , Reader Purpose
                    , Reader (Context (Binding usage) (Type (Binding usage)))
                    , State (Substitution (Type (Binding usage)))
                    ] effects
         , Monoid usage
         )
      => Term
      -> Type (Binding usage)
      -> Proof usage effects (Type (Binding usage))
check term expected = do
  actual <- infer term
  unify actual expected

infer :: ( Eq usage
         , Members '[ Exc (Error (Binding usage))
                    , Fresh
                    , Reader Purpose
                    , Reader (Context (Binding usage) (Type (Binding usage)))
                    , State (Substitution (Type (Binding usage)))
                    ] effects
         , Monoid usage
         )
      => Term
      -> Proof usage effects (Type (Binding usage))
infer term = case unTerm term of
  Var name                                 -> do
    context <- askContext
    maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  Intro i
    | Unit                  <- i -> pure unitT
    | Bool _                <- i -> pure boolT
    | Abs (var ::: ty) body <- i -> do
      ty' <- checkIsType ty
      let binding = Binding var zero
      body' <- binding ::: ty' >- infer body
      pure (binding ::: ty' .-> body')
    | Pair a b              <- i -> (.*) <$> infer a <*> infer b
    | UnitT                 <- i -> pure typeT
    | BoolT                 <- i -> pure typeT
    | TypeT                 <- i -> pure typeT
    | var ::: ty :-> body   <- i -> do
      (ty' :: Type (Binding usage)) <- checkIsType ty
      Binding var (zero @usage) ::: ty' >- checkIsType body $> typeT
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


runSubstitution :: Named var => Proof usage (State (Substitution (Type var)) ': effects) a -> Proof usage effects (a, Substitution (Type var))
runSubstitution = runState mempty


askPurpose :: (Effectful m, Member (Reader Purpose) effects) => m effects Purpose
askPurpose = ask
