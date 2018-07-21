{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module Manifold.Value where

import Data.Coerce
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor.Classes (showsBinaryWith)
import Manifold.Abstract.Address
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


instance Member (Resumable (ValueError address eval)) effects
      => Abstract.Value address (Value address eval) effects where
  construct n vs = pure (Data n vs)
  deconstruct (Data name values) = pure (name, values)
  deconstruct a = throwResumable (DeconstructError a)

runFunction :: ( Address address effects
               , Coercible eval (Eff effects)
               , Member Fresh effects
               , Member (Reader (Env address)) effects
               , Member (Resumable (ValueError address eval)) effects
               , Member (State (Store address (Value address eval))) effects
               , PureEffects effects
               )
            => Evaluator address (Value address eval) (Abstract.Function (Value address eval) ': effects) a
            -> Evaluator address (Value address eval) effects a
runFunction = interpret $ \case
  Abstract.Lambda n body -> do
    i <- fresh
    pure (Closure n (ClosureBody i (coerce (runFunction (Evaluator body)))))
  Abstract.Apply (Closure n (ClosureBody _ b)) a -> do
    addr <- alloc n
    assign addr a
    n .= addr $ coerce b
  Abstract.Apply f a -> throwResumable (ApplyError f a)

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
