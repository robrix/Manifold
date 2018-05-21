{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeOperators #-}
module Manifold.Type.Formation where

import Control.Monad.Effect
import Control.Monad.Effect.State
import Data.Semiring (zero)
import Manifold.Context
import Manifold.Expr
import Manifold.Proof

typeFormation :: ( Members '[ Exc (Error usage)
                            , CheckIsType usage
                            , State (Context usage)
                            ] effects
                 , Monoid usage
                 )
              => CheckIsType usage result
              -> Proof usage effects result
typeFormation (CheckIsType tm) = Type <$> case unTerm tm of
  UnitType -> pure UnitType
  BoolType -> pure BoolType
  TypeType -> pure TypeType
  (name, usage) ::: _S :-> _T -> do
    _S' <- checkIsType _S
    _T' <- (name, zero) ::: _S' >- checkIsType _T
    pure ((name, usage) ::: _S' :-> _T')
  _S :* _T -> (:*) <$> checkIsType _S <*> checkIsType _T
  Ann tm ty -> Ann <$> checkIsType tm <*> checkIsType ty
  _ -> noRuleToCheckIsType tm


-- | Extend the context with a local assumption.
(>-) :: Member (State (Context usage)) effects => Constraint usage (Type usage) -> Proof usage effects a -> Proof usage effects a
constraint >- proof = localState (:> constraint) proof

infixl 1 >-


checkIsType :: Member (CheckIsType usage) effects => Term usage -> Proof usage effects (Type usage)
checkIsType = send . CheckIsType


data PropositionalEquality usage result where
  (:==:) :: Type usage -> Type usage -> PropositionalEquality usage (Type usage)

data CheckIsType usage result where
  CheckIsType :: Term usage -> CheckIsType usage (Type usage)

runCheckIsType :: ( Members '[ Exc (Error usage)
                             , State (Context usage)
                             ] effects
                  , Monoid usage
                  )
               => Proof usage (CheckIsType usage ': effects) a
               -> Proof usage effects a
runCheckIsType = refine typeFormation
