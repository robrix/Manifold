{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, KindSignatures, TypeOperators #-}
module Manifold.Proof.Formation where

import Control.Monad.Effect
import Control.Monad.Effect.Internal
import Data.Semiring (zero)
import Manifold.Constraint
import Manifold.Context
import Manifold.Term.Elim
import Manifold.Type.Intro
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Type

runIsType :: ( Effects effects
             , Member (Exc (Error (Annotated usage))) effects
             , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects
             , Monoid usage
             )
          => Proof usage (IsType usage ': effects) a
          -> Proof usage effects a
runIsType = go . lowerEff
  where go (Return a) = pure a
        go (Effect (IsType ty) k) = runIsType $ case unType ty of
          Var name -> lookupType name *> Proof (k (tvar name))
          IntroT TypeT -> Proof (k typeT)
          IntroT (TypeC con ts) -> typeC con <$> traverse isType ts >>= Proof . k
          IntroT (var ::: _S :-> _T) -> do
            _S' <- isType _S
            let binding = Annotated var zero
            _T' <- binding ::: _S' >- isType _T
            Proof (k (binding ::: _S' .-> _T'))
          IntroT (_S :* _T) -> (.*) <$> isType _S <*> isType _T >>= Proof . k
          Elim (App f a) -> (#) <$> isType f <*> isType a >>= Proof . k
          _ -> noRuleToCheckIsType ty
        go (Other u k) = liftHandler runIsType u (Proof . k)


isType :: Member (IsType usage) effects => Type Name -> Proof usage effects (Type (Annotated usage))
isType = send . IsType

data IsType usage (m :: * -> *) result where
  IsType :: Type Name -> IsType usage m (Type (Annotated usage))


instance Effect (IsType usage) where
  handleState c dist (Request (IsType ty) k) = Request (IsType ty) (dist . (<$ c) . k)
