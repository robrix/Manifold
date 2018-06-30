{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, TypeOperators #-}
module Manifold.Proof where

import Control.Monad.Effect
import qualified Control.Monad.Effect.Exception as Exception
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Semilattice.Lower
import Manifold.Name.Annotated
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty
import Manifold.Term
import Manifold.Type

newtype Proof usage effects a = Proof { runProof :: Eff effects a }
  deriving (Applicative, Functor, Monad)


freeVariable :: (Member (Exc (Error (Annotated usage))) effects, Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects) => Name -> Proof usage effects a
freeVariable name = ask >>= throwError . FreeVariable name

cannotUnify :: (Member (Exc (Error var)) effects, Member (Reader (Context (Constraint var (Type var)))) effects) => Type var -> Type var -> Proof usage effects a
cannotUnify t1 t2 = ask >>= Exception.throwError . CannotUnify t1 t2

noRuleToCheckIsType :: (Member (Exc (Error (Annotated usage))) effects, Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects) => Type Name -> Proof usage effects a
noRuleToCheckIsType ty  = ask >>= throwError . NoRuleToCheckIsType ty

noRuleToInferType :: Member (Exc (Error (Annotated usage))) effects => Term Name -> Proof usage effects a
noRuleToInferType = throwError . NoRuleToInferType

unknownModule :: Member (Exc (Error (Annotated usage))) effects => Name -> Proof usage effects a
unknownModule = throwError . UnknownModule

throwError :: Member (Exc (Error (Annotated usage))) effects => Error (Annotated usage) -> Proof usage effects a
throwError = Exception.throwError

data Error var
  = FreeVariable Name (Context (Constraint var (Type var)))
  | CannotUnify (Type var) (Type var) (Context (Constraint var (Type var)))
  | NoRuleToCheckIsType (Type Name) (Context (Constraint var (Type var)))
  | NoRuleToInferType (Term Name)
  | UnknownModule Name
  deriving (Eq, Ord, Show)

instance Pretty var => Pretty (Error var) where
  prettyPrec d err = prettyParen (d > 0) $ prettyString "error:" <+> case err of
    FreeVariable name context -> prettyString "free variable:" <+> pretty name <+> prettyString "in" <+> pretty context
    CannotUnify t1 t2 context -> sep [ prettyString "cannot unify", pretty t1, prettyString "with", pretty t2, prettyString "in", pretty context ]
    NoRuleToCheckIsType t context -> prettyString "cannot prove" <+> pretty t <+> prettyString "is a valid type in context" <+> pretty context
    NoRuleToInferType t -> prettyString "cannot infer type of term" <+> pretty t
    UnknownModule name -> prettyString "unknown module:" <+> pretty name

runError :: Effects effects => Proof usage (Exc (Error (Annotated usage)) ': effects) a -> Proof usage effects (Either (Error (Annotated usage)) a)
runError = Exception.runError


runContext :: Effects effects => Proof usage (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage)))) ': effects) a -> Proof usage effects a
runContext = runReader lowerBound

askContext :: Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects => Proof usage effects (Context (Constraint (Annotated usage) (Type (Annotated usage))))
askContext = ask


-- | Extend the context with a local assumption.
(>-) :: Member (Reader (Context (Constraint (Annotated usage) recur))) effects => Constraint (Annotated usage) recur -> Proof usage effects a -> Proof usage effects a
constraint >- proof = local (|> constraint) proof

infixl 1 >-


lookupType :: ( Member (Exc (Error (Annotated usage))) effects
              , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) effects
              )
           => Name
           -> Proof usage effects (Type (Annotated usage))
lookupType name = do
  context <- askContext
  maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)


freshName :: Member Fresh effects => Proof usage effects Name
freshName = I <$> fresh
