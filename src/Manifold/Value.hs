{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, MultiParamTypeClasses, UndecidableInstances #-}
module Manifold.Value where

import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor.Classes (showsBinaryWith)
import Manifold.Abstract.Address
import Manifold.Abstract.Env
import Manifold.Abstract.Evaluator
import Manifold.Abstract.Store
import qualified Manifold.Abstract.Value as Abstract
import Manifold.Constraint
import Manifold.Context
import Manifold.Eval
import Manifold.Name
import Manifold.Pretty
import Manifold.Term (Term, freeVariables)

data Value address
  = Closure Name (ClosureBody (Term Name)) (Env address)
  | Data Name [Value address]
  deriving (Eq, Ord, Show)

data ClosureBody body = ClosureBody { closureId :: Int, closureBody :: body }


instance ( Address address effects
         , Member (Eval (Value address)) effects
         , Member Fresh effects
         , Member (Reader (Env address)) effects
         , Member (Resumable (ValueError address)) effects
         , Member (State (Store address (Value address))) effects
         )
      => Abstract.Value address (Value address) effects where
  lambda n body = do
    i <- fresh
    env <- contextFilter (((&&) <$> (/= n) <*> (`elem` freeVariables body)) . name) <$> ask
    pure (Closure n (ClosureBody i body) env)
  apply (Closure n (ClosureBody _ b) env) a = do
    addr <- alloc n
    assign addr a
    local (const (env |> (n ::: addr))) $ eval b
  apply f a = throwResumable (ApplyError f a)

  construct n vs = pure (Data n vs)
  deconstruct (Data name values) = pure (name, values)
  deconstruct a = throwResumable (DeconstructError a)

instance Pretty address => Pretty (Value address) where
  prettyPrec d = \case
    Closure name body env -> prettyParen (d > 0) $ backslash <+> prettyPrec 0 name <+> dot <+> brackets (prettyPrec 0 env) <> prettyPrec 0 body
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


data ValueError address result where
  ApplyError :: Value address -> Value address -> ValueError address (Value address)
  DeconstructError :: Value address -> ValueError address (Name, [Value address])

instance Pretty address => Pretty1 (ValueError address) where
  liftPrettyPrec _ d (ApplyError f a) = prettyParen (d > 0) $ prettyString "cannot apply" <+> pretty f <+> prettyString "to" <+> pretty a
  liftPrettyPrec _ d (DeconstructError v) = prettyParen (d > 0) $ prettyString "cannot deconstruct" <+> pretty v
