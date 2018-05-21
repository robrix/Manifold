module Manifold.Value where

import Manifold.Context
import Manifold.Expr hiding (Unit, Pair)
import Manifold.Name

data Value usage
  = Closure Name (Term usage) (Context usage (Value usage))
  | Unit
  | UnitT
  | Bool Bool
  | BoolT
  | TypeT
  | Pair (Value usage) (Value usage)
  | Product (Value usage) (Value usage)
  deriving (Eq, Ord, Show)
