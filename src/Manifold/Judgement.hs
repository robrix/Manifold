{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Judgement where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Functor (($>))
import Data.Semiring (Semiring(..), zero)
import Manifold.Context
import Manifold.Name
import Manifold.Presyntax
import Manifold.Proof
import Manifold.Substitution
import Manifold.Unification

typeFormation :: ( Members '[ Exc (Some (CheckIsType usage))
                            , CheckIsType usage
                            , Reader (Context usage)
                            ] effects
                 , Monoid usage
                 )
              => CheckIsType usage result
              -> Proof usage effects result
typeFormation (CheckIsType ty) = Type <$> case unType ty of
  UnitType -> pure UnitType
  BoolType -> pure BoolType
  (name, usage) ::: _S :-> _T -> do
    _S' <- checkIsType _S
    _T' <- (name, zero) ::: _S >- checkIsType _T
    pure ((name, usage) ::: _S' :-> _T')
  _ -> noRuleTo (CheckIsType ty)


typing :: ( Eq usage
          , Members '[ Check usage
                     , CheckIsType usage
                     , Exc (Some (Check usage))
                     , Exc (Some (Unify usage))
                     , Fresh
                     , Reader (Context usage)
                     , State (Substitution (Type usage))
                     ] effects
          , Monoid usage
          , Semiring usage
          )
       => Check usage result
       -> Proof usage effects result
typing (Check term expected) = do
  actual <- infer term
  runUnification $ unify actual expected
typing (Infer term) = Type <$> case unTerm term of
  Unit -> pure UnitType
  UnitType -> pure TypeType
  BoolType -> pure TypeType
  T -> pure BoolType
  F -> pure BoolType
  TypeType -> pure TypeType
  Var name -> contextFind name <$> ask >>= maybe (noRuleTo (Infer term)) (pure . unType . constraintType)
  (name, _) ::: ty :-> body -> do
    ty' <- checkIsType ty
    ((name, zero) ::: ty' >- check body (Type TypeType)) $> TypeType
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
    unType <$> runUnification (unify t' e')
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
  Ann tm ty -> unType <$> check tm ty


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context usage)) effects => Constraint usage -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (:> constraint) proof

infixl 1 >-


checkIsType :: Member (CheckIsType usage) effects => Type usage -> Proof usage effects (Type usage)
checkIsType = send . CheckIsType


data PropositionalEquality usage result where
  (:==:) :: Type usage -> Type usage -> PropositionalEquality usage (Type usage)

data CheckIsType usage result where
  CheckIsType :: Type usage -> CheckIsType usage (Type usage)


check :: Member (Check usage) effects => Term usage -> Type usage -> Proof usage effects (Type usage)
check tm ty = send (Check tm ty)

infer :: Member (Check usage) effects => Term usage -> Proof usage effects (Type usage)
infer = send . Infer

data Check usage result where
  Check :: Term usage -> Type usage -> Check usage (Type usage)
  Infer :: Term usage               -> Check usage (Type usage)
