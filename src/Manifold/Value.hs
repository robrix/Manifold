{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, MultiParamTypeClasses, PolyKinds, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Manifold.Value where

import Data.Coerce
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor.Classes (showsBinaryWith)
import Manifold.Abstract.Env
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import qualified Manifold.Abstract.Value as Abstract
import Manifold.Name
import Manifold.Pretty

data Value address eval
  = Closure Name (ClosureBody (eval (Value address eval)))
  | Data Name [Value address eval]
  deriving (Eq, Ord, Show)

data ClosureBody body = ClosureBody { closureId :: Int, closureBody :: body }


runFunction :: ( Carrier sig m
               , Coercible eval (Eff m)
               , Member (Allocator address (Value address eval)) sig
               , Member (Env address) sig
               , Member Fresh sig
               , Member (Resumable (ValueError address eval)) sig
               , Member (State (Store address (Value address eval))) sig
               , Ord address
               )
            => Evaluator address (Value address eval) (FunctionC (Evaluator address (Value address eval) m)) a
            -> Evaluator address (Value address eval) m a
runFunction = runFunctionC . interpret . runEvaluator

newtype FunctionC m a = FunctionC { runFunctionC :: m a }

instance ( Carrier sig m
         , Coercible eval (Eff m)
         , Member (Allocator address (Value address eval)) sig
         , Member (Env address) sig
         , Member Fresh sig
         , Member (Resumable (ValueError address eval)) sig
         , Member (State (Store address (Value address eval))) sig
         , Ord address
         )
      => Carrier (Abstract.Function (Value address eval) :+: sig) (FunctionC (Evaluator address (Value address eval) m)) where
  gen = FunctionC . gen
  alg = algF \/ (FunctionC . alg . handlePure runFunctionC)
    where algF :: Abstract.Function (Value address eval) (FunctionC (Evaluator address (Value address eval) m)) (FunctionC (Evaluator address (Value address eval) m) a)
               -> FunctionC (Evaluator address (Value address eval) m) a
          algF (Abstract.Lambda n body k) = FunctionC (do
            i <- fresh
            runFunctionC (k (Closure n (ClosureBody i (coerce (runFunctionC body))))))
          algF (Abstract.Apply (Closure n (ClosureBody _ b)) a k) = FunctionC ((do
            addr <- alloc n
            assign addr a
            n .= addr $ coerce b) >>= runFunctionC . k)
          algF (Abstract.Apply f a k) = FunctionC (throwResumable (ApplyError f a) >>= runFunctionC . k)

runData :: ( Member (Resumable (ValueError address eval)) sig
           , Carrier sig m
           )
        => Evaluator address (Value address eval) (DataC (Evaluator address (Value address eval) m)) a
        -> Evaluator address (Value address eval) m a
runData = runDataC . interpret . runEvaluator

newtype DataC m a = DataC { runDataC :: m a }

instance (Member (Resumable (ValueError address eval)) sig, Carrier sig m) => Carrier (Abstract.Data (Value address eval) :+: sig) (DataC (Evaluator address (Value address eval) m)) where
  gen = DataC . gen
  alg = algD \/ (DataC . alg . handlePure runDataC)
    where algD (Abstract.Construct name fields          k) = k (Data name fields)
          algD (Abstract.Deconstruct (Data name fields) k) = k (name, fields)
          algD (Abstract.Deconstruct value              k) = DataC (throwResumable (DeconstructError value) >>= runDataC . k)

instance Pretty address => Pretty (Value address eval) where
  prettyPrec d = \case
    Closure name body -> prettyParen (d > 0) $ backslash <+> prettyPrec 0 name <+> dot <+> prettyPrec 0 body
    Data (N "Unit") [] -> parens mempty
    Data c as -> prettyParen (d > 10) $ prettyPrec 10 c <> fold (map ((space <>) . prettyPrec 11) as)

instance Eq (ClosureBody body) where
  (==) = (==) `on` closureId

instance Ord (ClosureBody body) where
  compare = compare `on` closureId

instance Show (ClosureBody body) where
  showsPrec d (ClosureBody i _) = showParen (d > 10) $ showsBinaryWith showsPrec showsPrec "ClosureBody" d i '_'

instance Pretty (ClosureBody body) where
  prettyPrec d (ClosureBody i _) = prettyParen (d > 10) $ prettyString "ClosureBody" <+> pretty i


data ValueError address eval result where
  ApplyError :: Value address eval -> Value address eval -> ValueError address eval (Value address eval)
  DeconstructError :: Value address eval -> ValueError address eval (Name, [Value address eval])

instance Pretty address => Pretty1 (ValueError address eval) where
  liftPrettyPrec _ d (ApplyError f a) = prettyParen (d > 0) $ prettyString "cannot apply" <+> pretty f <+> prettyString "to" <+> pretty a
  liftPrettyPrec _ d (DeconstructError v) = prettyParen (d > 0) $ prettyString "cannot deconstruct" <+> pretty v
