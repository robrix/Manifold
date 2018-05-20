{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Judgement where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Functor (($>))
import Data.Semiring (Semiring(..), zero)
import Manifold.Context
import Manifold.Presyntax
import Manifold.Proof
import Manifold.Unification

typeFormation :: ( Members '[ Exc (Some (CheckIsType usage))
                            , CheckIsType usage
                            , Reader (Context usage)
                            ] effects
                 , Monoid usage
                 )
              => CheckIsType usage result
              -> Proof usage effects ()
typeFormation (CheckIsType ty) = case unType ty of
  BoolType -> pure ()
  (x, _) ::: _S :-> _T -> do
    checkIsType _S
    (x, zero) ::: _S >- checkIsType _T
  _ -> noRuleTo (CheckIsType ty)


typing :: ( Eq usage
          , Members '[ Check usage
                     , CheckIsType usage
                     , Exc (Some (Check usage))
                     , Exc (Some (Unify usage))
                     , Fresh
                     , Reader (Context usage)
                     ] effects
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
  a :* b -> checkIsType a *> checkIsType b $> TypeType
  Pair a b -> (:*) <$> infer a <*> infer b
  Ann tm ty -> unType <$> check tm ty
  _ -> noRuleTo (Infer term)


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context usage)) effects => Constraint usage -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (:> constraint) proof

infixl 1 >-


checkIsType :: Member (CheckIsType usage) effects => Type usage -> Proof usage effects ()
checkIsType = send . CheckIsType


data PropositionalEquality usage result where
  (:==:) :: Type usage -> Type usage -> PropositionalEquality usage (Type usage)

data CheckIsType usage result where
  CheckIsType :: Type usage -> CheckIsType usage ()


check :: Member (Check usage) effects => Term usage -> Type usage -> Proof usage effects (Type usage)
check tm ty = send (Check tm ty)

infer :: Member (Check usage) effects => Term usage -> Proof usage effects (Type usage)
infer = send . Infer

data Check usage result where
  Check :: Term usage -> Type usage -> Check usage (Type usage)
  Infer :: Term usage               -> Check usage (Type usage)
