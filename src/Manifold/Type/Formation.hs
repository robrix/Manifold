{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Manifold.Type.Formation where

import Control.Monad.Effect
import Data.Semiring (zero)
import Manifold.Context
import Manifold.Expr
import Manifold.Proof

checkIsType :: ( Members '[ Exc (Error usage)
                          , Reader (Context usage (Type usage))
                          ] effects
               , Monoid usage
               )
            => Term usage
            -> Proof usage effects (Type usage)
checkIsType term = Type <$> case unTerm term of
  UnitType -> pure UnitType
  BoolType -> pure BoolType
  TypeType -> pure TypeType
  (name, usage) ::: _S :-> _T -> do
    _S' <- checkIsType _S
    _T' <- (name, zero) ::: _S' >- checkIsType _T
    pure ((name, usage) ::: _S' :-> _T')
  _S :* _T -> (:*) <$> checkIsType _S <*> checkIsType _T
  Ann tm ty -> Ann <$> checkIsType tm <*> checkIsType ty
  _ -> noRuleToCheckIsType term
