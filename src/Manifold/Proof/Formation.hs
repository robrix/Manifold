{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Manifold.Proof.Formation where

import Control.Monad.Effect
import Data.Semiring (zero)
import Manifold.Constraint
import Manifold.Context
import Manifold.Type.Intro
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Type

checkIsType :: ( Members '[ Exc (Error (Annotated usage))
                          , Reader (Context (Annotated usage) (Type (Annotated usage)))
                          ] effects
               , Monoid usage
               )
            => Type Name
            -> Proof usage effects (Type (Annotated usage))
checkIsType ty = case unType ty of
  Var name -> do
    context <- askContext
    maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)
  IntroT UnitT -> pure unitT
  IntroT BoolT -> pure boolT
  IntroT TypeT -> pure typeT
  IntroT (TypeC c ts) -> typeC c <$> traverse checkIsType ts
  IntroT (var ::: _S :-> _T) -> do
    _S' <- checkIsType _S
    let binding = Annotated var zero
    _T' <- binding ::: _S' >- checkIsType _T
    pure (binding ::: _S' .-> _T')
  IntroT (_S :* _T) -> (.*) <$> checkIsType _S <*> checkIsType _T
  _ -> noRuleToCheckIsType ty
