module Manifold.Presyntax where

import Manifold.Name

data Constraint usage = Binding usage ::: Type usage
  deriving (Eq, Ord, Show)

infix 6 :::

data Binding usage = Name :@ usage
  deriving (Eq, Ord, Show)

infix 7 :@


data Type usage
  = Bool
  | Type
  | Constraint usage :-> Type usage
  deriving (Eq, Ord, Show)


data Term
  = T
  | F
  deriving (Eq, Ord, Show)
