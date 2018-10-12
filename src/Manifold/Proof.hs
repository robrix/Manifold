{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, PolyKinds, TypeOperators, UndecidableInstances #-}
module Manifold.Proof where

import Control.Effect
import Data.Semilattice.Lower
import Manifold.Name.Annotated
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty
import Manifold.Term
import Manifold.Type

newtype Proof usage carrier a = Proof { runProof :: Eff carrier a }
  deriving (Applicative, Functor, Monad)

instance Carrier sig carrier => Carrier sig (Proof usage carrier) where
  gen = pure
  alg op = Proof (alg (handlePure runProof op))

instance (Carrier sig carrier, Effect sig) => Effectful sig (Proof usage carrier)



freeVariable :: (Member (Error (ProofError (Annotated usage))) sig, Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig, Carrier sig m) => Name -> Proof usage m a
freeVariable name = ask >>= throwProofError . FreeVariable name

cannotUnify :: (Member (Error (ProofError var)) sig, Member (Reader (Context (Constraint var (Type var)))) sig, Carrier sig m) => Type var -> Type var -> Proof usage m a
cannotUnify t1 t2 = ask >>= throwError . CannotUnify t1 t2

noRuleToCheckIsType :: (Member (Error (ProofError (Annotated usage))) sig, Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig, Carrier sig m) => Type Name -> Proof usage m a
noRuleToCheckIsType ty  = ask >>= throwProofError . NoRuleToCheckIsType ty

noRuleToInferType :: (Member (Error (ProofError (Annotated usage))) sig, Carrier sig m) => Term Name -> Proof usage m a
noRuleToInferType = throwProofError . NoRuleToInferType

unknownModule :: (Member (Error (ProofError (Annotated usage))) sig, Carrier sig m) => Name -> Proof usage m a
unknownModule = throwProofError . UnknownModule

throwProofError :: (Member (Error (ProofError (Annotated usage))) sig, Carrier sig m) => ProofError (Annotated usage) -> Proof usage m a
throwProofError = throwError

data ProofError var
  = FreeVariable Name (Context (Constraint var (Type var)))
  | CannotUnify (Type var) (Type var) (Context (Constraint var (Type var)))
  | NoRuleToCheckIsType (Type Name) (Context (Constraint var (Type var)))
  | NoRuleToInferType (Term Name)
  | UnknownModule Name
  deriving (Eq, Ord, Show)

instance Pretty var => Pretty (ProofError var) where
  prettyPrec d err = prettyParen (d > 0) $ prettyString "error:" <+> case err of
    FreeVariable name context -> prettyString "free variable:" <+> pretty name <+> prettyString "in" <+> pretty context
    CannotUnify t1 t2 context -> sep [ prettyString "cannot unify", pretty t1, prettyString "with", pretty t2, prettyString "in", pretty context ]
    NoRuleToCheckIsType t context -> prettyString "cannot prove" <+> pretty t <+> prettyString "is a valid type in context" <+> pretty context
    NoRuleToInferType t -> prettyString "cannot infer type of term" <+> pretty t
    UnknownModule name -> prettyString "unknown module:" <+> pretty name

runProofError :: Effectful sig m => Proof usage (ErrorC (ProofError (Annotated usage)) (Proof usage m)) a -> Proof usage m (Either (ProofError (Annotated usage)) a)
runProofError = runError . runProof


runContext :: Carrier sig m => Proof usage (ReaderC (Context (Constraint (Annotated usage) (Type (Annotated usage)))) (Proof usage m)) a -> Proof usage m a
runContext = runReader lowerBound . runProof

askContext :: (Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig, Carrier sig m) => Proof usage m (Context (Constraint (Annotated usage) (Type (Annotated usage))))
askContext = ask


-- | Extend the context with a local assumption.
(>-) :: (Member (Reader (Context (Constraint (Annotated usage) recur))) sig, Carrier sig m) => Constraint (Annotated usage) recur -> Proof usage m a -> Proof usage m a
constraint >- proof = local (|> constraint) proof

infixl 1 >-


lookupType :: ( Member (Error (ProofError (Annotated usage))) sig
              , Member (Reader (Context (Constraint (Annotated usage) (Type (Annotated usage))))) sig
              , Carrier sig m
              )
           => Name
           -> Proof usage m (Type (Annotated usage))
lookupType name = do
  context <- askContext
  maybe (freeVariable name) (pure . constraintValue) (contextLookup name context)


freshName :: (Member Fresh sig, Carrier sig m) => Proof usage m Name
freshName = I <$> fresh
