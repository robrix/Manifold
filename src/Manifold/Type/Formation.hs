{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Manifold.Type.Formation where

import Control.Monad.Effect
import Data.Semiring (zero)
import Manifold.Binding
import Manifold.Constraint
import Manifold.Context
import Manifold.Expr
import Manifold.Proof
import Manifold.Term
import Manifold.Type

checkIsType :: ( Members '[ Exc (Error (Binding usage))
                          , Reader (Context usage (Type (Binding usage)))
                          ] effects
               , Monoid usage
               )
            => Term
            -> Proof usage effects (Type (Binding usage))
checkIsType term = case unTerm term of
  Intro UnitT -> pure (tintro UnitT)
  Intro BoolT -> pure (tintro BoolT)
  Intro TypeT -> pure (tintro TypeT)
  Intro (var ::: _S :-> _T) -> do
    _S' <- checkIsType _S
    _T' <- Binding var zero ::: _S' >- checkIsType _T
    pure (Binding var zero ::: _S' .-> _T')
  Intro (_S :* _T) -> (.*) <$> checkIsType _S <*> checkIsType _T
  _ -> noRuleToCheckIsType term
