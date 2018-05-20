{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Proof where

import Control.Monad.Effect
import qualified Control.Monad.Effect.Exception as Exception
import Control.Monad.Effect.Reader
import Manifold.Context
import Manifold.Name
import Manifold.Presyntax

newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)


freeVariable :: Members '[Exc (Error usage), Reader (Context usage)] effects => Name -> Proof usage effects a
freeVariable name = ask >>= throwError . FreeVariable name

cannotUnify :: Member (Exc (Error usage)) effects => Type usage -> Type usage -> Proof usage effects a
cannotUnify t1 t2 = throwError (CannotUnify t1 t2)

noRuleToCheckIsType :: Member (Exc (Error usage)) effects => Term usage -> Proof usage effects a
noRuleToCheckIsType = throwError . NoRuleToCheckIsType

throwError :: Member (Exc (Error usage)) effects => Error usage -> Proof usage effects a
throwError = Exception.throwError

data Error usage
  = FreeVariable Name (Context usage)
  | CannotUnify (Type usage) (Type usage)
  | NoRuleToCheckIsType (Term usage)
  deriving (Eq, Ord, Show)

runError :: Proof usage (Exc (Error usage) ': effects) a -> Proof usage effects (Either (Error usage) a)
runError = Exception.runError

runContext :: Proof usage (Reader (Context usage) ': effects) a -> Proof usage effects a
runContext = runReader Empty
