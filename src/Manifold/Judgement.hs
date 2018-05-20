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
              -> Proof usage effects ()
typeFormation (CheckIsType ty) = case unType ty of
  Bool -> pure ()
  (x, _) ::: _S :-> _T -> do
    checkIsType _S
    (x, zero) ::: _S >- checkIsType _T
  _ -> noRuleTo (CheckIsType ty)


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context usage)) effects => Constraint usage -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (:> constraint) proof

infixl 1 >-


checkIsType :: Member (Proposition usage) effects => Type usage -> Proof usage effects ()
checkIsType = send . CheckIsType


noRuleTo :: Member (Exc (Some (proposition usage))) effects => proposition usage result -> Proof usage effects a
noRuleTo = throwError . Some


data PropositionalEquality usage result where
  (:==:) :: Type usage -> Type usage -> PropositionalEquality usage (Type usage)

data Proposition usage result where
  CheckIsType :: Type usage -> Proposition usage ()

data Some proposition where
  Some :: proposition result -> Some proposition


infer :: Member (Check usage) effects => Term usage -> Proof usage effects (Type usage)
infer = send . Infer

data Check usage result where
  Check :: Term usage -> Type usage -> Check usage (Type usage)
  Infer :: Term usge                -> Check usage (Type usage)


newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)
