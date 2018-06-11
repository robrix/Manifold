{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Manifold.Proof.Formation where

import Control.Monad.Effect
import Data.Semiring (zero)
import Manifold.Constraint
import Manifold.Context
import Manifold.Term.Elim
import Manifold.Type.Intro
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Type

checkIsType :: ( Member (Exc (Error (Annotated usage))) effects
               , Member (Reader (Context (Annotated usage) (Type (Annotated usage)))) effects
               , Monoid usage
               )
            => Type Name
            -> Proof usage effects (Type (Annotated usage))
checkIsType ty = case unType ty of
  Var name -> tvar name <$ lookupType name
  IntroT TypeT -> pure typeT
  IntroT (TypeC con ts) -> typeC con <$> traverse checkIsType ts
  IntroT (var ::: _S :-> _T) -> do
    _S' <- checkIsType _S
    let binding = Annotated var zero
    _T' <- binding ::: _S' >- checkIsType _T
    pure (binding ::: _S' .-> _T')
  IntroT (_S :* _T) -> (.*) <$> checkIsType _S <*> checkIsType _T
  Elim (App f a) -> (#) <$> checkIsType f <*> checkIsType a
  _ -> noRuleToCheckIsType ty
