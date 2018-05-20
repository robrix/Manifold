{-# LANGUAGE DataKinds, FlexibleContexts, GADTs #-}
module Manifold.Judgement where

import Control.Monad.Effect
import Control.Monad.Effect.Exception
import Control.Monad.Effect.Reader
import Data.Semiring (zero)
import Manifold.Context
import Manifold.Presyntax

data Proposition usage result where
  (:==:) :: Type usage -> Type usage -> Proposition usage ()
  IsType :: Type usage -> Proposition usage ()

isType :: Member (Proposition usage) effects => Type usage -> Eff effects ()
isType = send . IsType

data SomeProposition usage where
  SomeProposition :: Proposition usage result -> SomeProposition usage

typeFormation :: Monoid usage => Proposition usage result -> Proof usage ()
typeFormation prop = case prop of
  IsType (Type Bool) -> pure ()
  IsType (Type (((x, _) ::: _S) :-> _T)) -> do
    isType _S
    (x, zero) ::: _S >- isType _T
  other -> throwError (SomeProposition other)

type Proof usage = Eff '[Reader (Context usage), Proposition usage, Exc (SomeProposition usage)]

infixl 1 >-
(>-) :: Member (Reader (Context usage)) effects => Constraint usage -> Eff effects a -> Eff effects a
constraint >- proof = local (:> constraint) proof
