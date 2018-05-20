module Manifold.Presyntax where

import Manifold.Name

data Constraint usage = (Name, usage) ::: Type usage
  deriving (Eq, Ord, Show)

infix 5 :::


data Expr usage recur
  = Bool
  | T
  | F
  | Set
  | Constraint usage :-> recur
  | Var Name
  deriving (Eq, Ord, Show)

newtype Type usage = Type { unType :: Expr usage (Type usage) }
  deriving (Eq, Ord, Show)

newtype Term usage = Term { unTerm :: Expr usage (Term usage) }
  deriving (Eq, Ord, Show)
