{-# LANGUAGE LambdaCase #-}
module Manifold.Value where

import Data.Foldable (fold)
import Manifold.Constraint
import Manifold.Context
import Manifold.Name
import Manifold.Pretty
import Manifold.Term (Term)

type Environment = Context (Constraint Name Value)

data Value
  = Closure Name (Term Name) Environment
  | Data Name [Value]
  deriving (Eq, Ord, Show)


instance Pretty Value where
  prettyPrec d = \case
    Closure name body env -> prettyParen (d > 0) $ backslash <+> prettyPrec 0 name <+> dot <+> brackets (prettyPrec 0 env) <> prettyPrec 0 body
    Data (N "Unit") [] -> parens mempty
    Data c as -> prettyParen (d > 10) $ prettyPrec 10 c <> fold (map ((space <>) . prettyPrec 11) as)
