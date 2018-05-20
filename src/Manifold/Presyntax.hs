module Manifold.Presyntax where

import Manifold.Name

data Constraint usage = (Name, usage) ::: Type usage
  deriving (Eq, Ord, Show)

infix 5 :::


data Type usage
  = Bool
  | Type
  | Constraint usage :-> Type usage
  deriving (Eq, Ord, Show)


data Term
  = T
  | F
  | Var Name
  deriving (Eq, Ord, Show)
