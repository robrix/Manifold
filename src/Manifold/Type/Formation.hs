{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Manifold.Type.Formation where

import Control.Monad.Effect
import Data.Semiring (zero)
import Manifold.Context
import Manifold.Expr
import Manifold.Proof
import Manifold.Term

checkIsType :: ( Members '[ Exc (Error usage)
                          , Reader (Context usage (Type usage))
                          ] effects
               , Monoid usage
               )
            => Term usage
            -> Proof usage effects (Type usage)
checkIsType term = case unTerm term of
  Intro UnitT -> pure (tintro UnitT)
  Intro BoolT -> pure (tintro BoolT)
  Intro TypeT -> pure (tintro TypeT)
  Intro ((name, usage) ::: _S :-> _T) -> do
    _S' <- checkIsType _S
    _T' <- (name, zero) ::: _S' >- checkIsType _T
    pure ((name, usage) ::: _S' .-> _T')
  Intro (_S :* _T) -> (.*) <$> checkIsType _S <*> checkIsType _T
  _ -> noRuleToCheckIsType term
