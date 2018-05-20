{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Judgement where

import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Semiring (Semiring(..), zero)
import Manifold.Context
import Manifold.Presyntax
import Manifold.Proof
import Manifold.Unification

typeFormation :: ( Members '[ Exc (Some (Proposition usage))
                            , Proposition usage
                            , Reader (Context usage)
                            ] effects
                 , Monoid usage
                 )
              => Proposition usage result
              -> Proof usage effects ()
typeFormation (CheckIsType ty) = case unType ty of
  BoolType -> pure ()
  (x, _) ::: _S :-> _T -> do
    checkIsType _S
    (x, zero) ::: _S >- checkIsType _T
  _ -> noRuleTo (CheckIsType ty)


typing :: ( Eq usage
          , Members '[ Check usage
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
typing (Infer term) = case unTerm term of
  Unit -> pure (Type UnitType)
  UnitType -> pure (Type TypeType)
  BoolType -> pure (Type TypeType)
  T -> pure (Type BoolType)
  F -> pure (Type BoolType)
  TypeType -> pure (Type TypeType)
  Ann tm ty -> check tm ty
  _ -> noRuleTo (Infer term)


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context usage)) effects => Constraint usage -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (:> constraint) proof

infixl 1 >-


checkIsType :: Member (Proposition usage) effects => Type usage -> Proof usage effects ()
checkIsType = send . CheckIsType


data PropositionalEquality usage result where
  (:==:) :: Type usage -> Type usage -> PropositionalEquality usage (Type usage)

data Proposition usage result where
  CheckIsType :: Type usage -> Proposition usage ()


check :: Member (Check usage) effects => Term usage -> Type usage -> Proof usage effects (Type usage)
check tm ty = send (Check tm ty)

infer :: Member (Check usage) effects => Term usage -> Proof usage effects (Type usage)
infer = send . Infer

data Check usage result where
  Check :: Term usage -> Type usage -> Check usage (Type usage)
  Infer :: Term usage               -> Check usage (Type usage)
