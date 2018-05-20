{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving #-}
module Manifold.Judgement where

import Control.Monad.Effect
import Control.Monad.Effect.Exception
import Control.Monad.Effect.Reader
import Data.Semiring (zero)
import Manifold.Context
import Manifold.Presyntax

typeFormation :: ( Members '[ Exc (Some (Proposition usage))
                            , Proposition usage
                            , Reader (Context usage)
                            ] effects
                 , Monoid usage
                 )
              => Proposition usage result
              -> Eff effects ()
typeFormation prop = case prop of
  IsType (Type Bool) -> pure ()
  IsType (Type ((x, _) ::: _S :-> _T)) -> do
    isType _S
    (x, zero) ::: _S >- isType _T
  other -> cannotProve other


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context usage)) effects => Constraint usage -> Eff effects a -> Eff effects a
constraint >- proof = local (:> constraint) proof

infixl 1 >-


isType :: Member (Proposition usage) effects => Type usage -> Eff effects ()
isType = send . IsType


cannotProve :: Member (Exc (Some proposition)) effects => proposition result -> Eff effects a
cannotProve = throwError . Some


data PropositionalEquality usage result where
  (:==:) :: Type usage -> Type usage -> PropositionalEquality usage ()

data Proposition usage result where
  IsType :: Type usage -> Proposition usage ()

data Some proposition where
  Some :: proposition result -> Some proposition


data Check usage result where
  Check :: Term usage -> Type usage -> Check usage (Type usage)
  Infer :: Term usge                -> Check usage (Type usage)


newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)
