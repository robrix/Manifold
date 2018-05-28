{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Proof where

import Control.Monad.Effect
import qualified Control.Monad.Effect.Exception as Exception
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Manifold.Name.Annotated
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty
import Manifold.Term
import Manifold.Type

newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Effectful, Functor, Monad)


freeVariable :: Member (Exc (Error (Annotated usage))) effects => Name -> Proof usage effects a
freeVariable = throwError . FreeVariable

cannotUnify :: Member (Exc (Error var)) effects => Type var -> Type var -> Proof usage effects a
cannotUnify t1 t2 = Exception.throwError (CannotUnify t1 t2)

noRuleToCheckIsType :: Member (Exc (Error (Annotated usage))) effects => Type Name -> Proof usage effects a
noRuleToCheckIsType = throwError . NoRuleToCheckIsType

noRuleToInferType :: Member (Exc (Error (Annotated usage))) effects => Term Name -> Proof usage effects a
noRuleToInferType = throwError . NoRuleToInferType

throwError :: Member (Exc (Error (Annotated usage))) effects => Error (Annotated usage) -> Proof usage effects a
throwError = Exception.throwError

data Error var
  = FreeVariable Name
  | CannotUnify (Type var) (Type var)
  | NoRuleToCheckIsType (Type Name)
  | NoRuleToInferType (Term Name)
  deriving (Eq, Ord, Show)

instance Pretty var => Pretty (Error var) where
  prettyPrec d err = showParen (d > 0) $ showString "error: " . case err of
    FreeVariable name -> showString "free variable: " . prettys name
    CannotUnify t1 t2 -> showString "cannot unify\n" . prettys t1 . showString "\nwith\n" . prettys t2
    NoRuleToCheckIsType t -> showString "cannot prove " . prettys t . showString " is a valid type"
    NoRuleToInferType t -> showString "cannot infer type of term " . prettys t

runError :: Proof usage (Exc (Error var) ': effects) a -> Proof usage effects (Either (Error var) a)
runError = Exception.runError


runContext :: Proof usage (Reader (Context (Annotated usage) (Type (Annotated usage))) ': effects) a -> Proof usage effects a
runContext = runReader emptyContext

askContext :: Member (Reader (Context (Annotated usage) (Type (Annotated usage)))) effects => Proof usage effects (Context (Annotated usage) (Type (Annotated usage)))
askContext = ask


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context var recur)) effects => Constraint var recur -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (|> constraint) proof

infixl 1 >-


freshName :: Member Fresh effects => Proof usage effects Name
freshName = I <$> fresh
