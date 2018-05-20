{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Type.Checking where

import Control.Monad.Effect
import Control.Monad.Effect.Exception
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Functor (($>))
import Data.Semiring (Semiring(..), zero)
import GHC.Generics ((:+:)(..))
import Manifold.Context
import Manifold.Name
import Manifold.Presyntax
import Manifold.Proof
import Manifold.Substitution
import Manifold.Type.Formation
import Manifold.Unification

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
  Ann tm ty -> checkIsType ty >>= fmap unType . check tm


check :: Member (Check usage) effects => Term usage -> Type usage -> Proof usage effects (Type usage)
check tm ty = send (Check tm ty)

infer :: Member (Check usage) effects => Term usage -> Proof usage effects (Type usage)
infer = send . Infer

data Check usage result where
  Check :: Term usage -> Type usage -> Check usage (Type usage)
  Infer :: Term usage               -> Check usage (Type usage)

runCheck :: ( Eq usage
            , Members '[ CheckIsType usage
                       , Exc (Some (Check usage))
                       , Exc (Some (Unify usage))
                       , Fresh
                       , Reader (Context usage)
                       , State (Substitution (Type usage))
                       ] effects
            , Monoid usage
            , Semiring usage
            )
         => Proof usage (Check usage ': effects) a
         -> Proof usage effects a
runCheck = refine typing

runContext :: Proof usage (Reader (Context usage) ': effects) a -> Proof usage effects a
runContext = runReader Empty

runErrors :: Proof usage (Exc (Some (Unify usage)) ': Exc (Some (Check usage)) ': Exc (Some (CheckIsType usage)) ': effects) a -> Proof usage effects (Either (Some (Unify usage :+: Check usage :+: CheckIsType usage)) a)
runErrors = fmap reassoc . runError . runError . runError
  where reassoc (Left (Some checkIsType)) = Left (Some (R1 (R1 checkIsType)))
        reassoc (Right (Left (Some check))) = Left (Some (R1 (L1 check)))
        reassoc (Right (Right (Left (Some unify)))) = Left (Some (L1 unify))
        reassoc (Right (Right (Right a))) = Right a
