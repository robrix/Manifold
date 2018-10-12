{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.Proof.Formation where

import Control.Effect
import Data.Semiring (zero)
import Manifold.Constraint
import Manifold.Context
import Manifold.Term.Elim
import Manifold.Type.Intro
import Manifold.Name
import Manifold.Name.Annotated
import Manifold.Proof
import Manifold.Type

runIsType :: ( Member (Error (ProofError (Annotated usage))) sig
             , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
             , Monoid usage
             , Carrier sig m
             )
          => Proof usage (IsTypeH usage (Proof usage m)) a
          -> Proof usage m a
runIsType = runIsTypeH . interpret . runProof

newtype IsTypeH usage m a = IsTypeH { runIsTypeH :: m a }

instance (Carrier sig m, Member (Error (ProofError (Annotated usage))) sig, Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig, Monoid usage) => Carrier (IsType usage :+: sig) (IsTypeH usage (Proof usage m)) where
  gen = IsTypeH . gen
  alg = algI \/ (IsTypeH . alg . handlePure runIsTypeH)
    where algI (IsType ty k) = IsTypeH $ case unType ty of
            Var name -> lookupType name *> runIsTypeH (k (tvar name))
            IntroT TypeT -> runIsTypeH (k typeT)
            IntroT (TypeC con ts) -> typeC con <$> traverse (runIsType . isType) ts >>= runIsTypeH . k
            IntroT (var ::: _S :-> _T) -> do
              _S' <- runIsType (isType _S)
              let binding = Annotated var zero
              _T' <- binding ::: _S' >- runIsType (isType _T)
              runIsTypeH (k (binding ::: _S' .-> _T'))
            IntroT (_S :* _T) -> runIsType ((.*) <$> isType _S <*> isType _T) >>= runIsTypeH . k
            Elim (App f a) -> runIsType ((#) <$> isType f <*> isType a) >>= runIsTypeH . k
            _ -> noRuleToCheckIsType ty


isType :: (Member (IsType usage) sig, Carrier sig m) => Type Name -> Proof usage m (Type (Annotated usage))
isType = send . flip IsType gen

data IsType usage m k
  = IsType (Type Name) (Type (Annotated usage) -> k)
  deriving (Functor)

instance HFunctor (IsType usage) where
  hfmap _ (IsType ty k) = IsType ty k

instance Effect (IsType usage) where
  handle state handler (IsType ty k) = IsType ty (handler . (<$ state) . k)
