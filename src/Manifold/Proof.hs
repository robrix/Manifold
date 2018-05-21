{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Proof where

import Control.Monad.Effect
import qualified Control.Monad.Effect.Exception as Exception
import Control.Monad.Effect.Reader
import Manifold.Binding
import Manifold.Constraint
import Manifold.Context
import Manifold.Term
import Manifold.Type
import Manifold.Name

newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)


freeVariable :: Member (Exc (Error (Binding usage))) effects => Name -> Proof usage effects a
freeVariable = throwError . FreeVariable

cannotUnify :: Member (Exc (Error var)) effects => Type var -> Type var -> Proof usage effects a
cannotUnify t1 t2 = Exception.throwError (CannotUnify t1 t2)

noRuleToCheckIsType :: Member (Exc (Error (Binding usage))) effects => Term -> Proof usage effects a
noRuleToCheckIsType = throwError . NoRuleToCheckIsType

throwError :: Member (Exc (Error (Binding usage))) effects => Error (Binding usage) -> Proof usage effects a
throwError = Exception.throwError

data Error var
  = FreeVariable Name
  | CannotUnify (Type var) (Type var)
  | NoRuleToCheckIsType Term
  deriving (Eq, Ord, Show)

runError :: Proof usage (Exc (Error var) ': effects) a -> Proof usage effects (Either (Error var) a)
runError = Exception.runError

catchError :: Member (Exc (Error usage)) effects => Proof usage effects a -> (Error usage -> Proof usage effects a) -> Proof usage effects a
catchError = Exception.catchError


runContext :: Proof usage (Reader (Context usage recur) ': effects) a -> Proof usage effects a
runContext = runReader emptyContext

askContext :: Member (Reader (Context usage (Type (Binding usage)))) effects => Proof usage effects (Context usage (Type (Binding usage)))
askContext = ask


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context usage recur)) effects => Constraint (Binding usage) recur -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (|> constraint) proof

infixl 1 >-
