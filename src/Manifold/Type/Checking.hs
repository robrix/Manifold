{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Manifold.Type.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.State
import Data.Functor (($>))
import Data.Semiring (Semiring(..), zero)
import Manifold.Context
import Manifold.Expr
import Manifold.Name
import Manifold.Proof
import Manifold.Substitution
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
infer term = Type <$> case unTerm term of
  Unit -> pure UnitType
  UnitType -> pure TypeType
  BoolType -> pure TypeType
  T -> pure BoolType
  F -> pure BoolType
  TypeType -> pure TypeType
  Var name -> contextFind name <$> askContext >>= maybe (freeVariable name) (pure . unType . constraintValue)
  (name, _) ::: ty :-> body -> do
    ty' <- checkIsType ty
    ((name, zero) ::: ty' >- checkIsType body) $> TypeType
  Abs ((name, usage) ::: ty) body -> do
    ty' <- checkIsType ty
    body' <- (name, usage) ::: ty' >- infer body
    pure ((name, usage) ::: ty' :-> body')
  App f a -> do
    n <- I <$> fresh
    t1 <- Var . I <$> fresh
    t2 <- Var . I <$> fresh
    _ <- check f (Type ((n, zero) ::: Type t1 :-> Type t2))
    _ <- check a (Type t1)
    pure t2
  If c t e -> do
    _ <- check c (Type BoolType)
    t' <- infer t
    e' <- infer e
    unType <$> unify t' e'
  a :* b -> checkIsType a *> checkIsType b $> TypeType
  Pair a b -> (:*) <$> infer a <*> infer b
  ExL a -> do
    t1 <- Var . I <$> fresh
    t2 <- Var . I <$> fresh
    _ <- check a (Type (Type t1 :* Type t2))
    pure t1
  ExR a -> do
    t1 <- Var . I <$> fresh
    t2 <- Var . I <$> fresh
    _ <- check a (Type (Type t1 :* Type t2))
    pure t2
  Ann tm ty -> checkIsType ty >>= fmap unType . check tm


runSubstitution :: Proof usage (State (Substitution (Type usage)) ': effects) a -> Proof usage effects (a, Substitution (Type usage))
runSubstitution = runState mempty
