{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Proof where

import Control.Monad.Effect
import qualified Control.Monad.Effect.Exception as Exception
import Control.Monad.Effect.Reader
import Manifold.Constraint
import Manifold.Context
import Manifold.Term
import Manifold.Type
import Manifold.Name

newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)


freeVariable :: Member (Exc (Error usage)) effects => Name -> Proof usage effects a
freeVariable = throwError . FreeVariable

cannotUnify :: Member (Exc (Error usage)) effects => Type usage -> Type usage -> Proof usage effects a
cannotUnify t1 t2 = throwError (CannotUnify t1 t2)

noRuleToCheckIsType :: Member (Exc (Error usage)) effects => Term usage -> Proof usage effects a
noRuleToCheckIsType = throwError . NoRuleToCheckIsType

throwError :: Member (Exc (Error usage)) effects => Error usage -> Proof usage effects a
throwError = Exception.throwError

data Error usage
  = FreeVariable Name
  | CannotUnify (Type usage) (Type usage)
  | NoRuleToCheckIsType (Term usage)
  deriving (Eq, Ord, Show)

runError :: Proof usage (Exc (Error usage) ': effects) a -> Proof usage effects (Either (Error usage) a)
runError = Exception.runError

catchError :: Member (Exc (Error usage)) effects => Proof usage effects a -> (Error usage -> Proof usage effects a) -> Proof usage effects a
catchError = Exception.catchError


runContext :: Proof usage (Reader (Context usage recur) ': effects) a -> Proof usage effects a
runContext = runReader Empty

askContext :: Member (Reader (Context usage (Type usage))) effects => Proof usage effects (Context usage (Type usage))
askContext = ask


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context usage recur)) effects => Constraint usage recur -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (:> constraint) proof

infixl 1 >-
