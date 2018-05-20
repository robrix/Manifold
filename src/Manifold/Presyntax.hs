module Manifold.Presyntax where

import Manifold.Name

data Constraint usage = Binding usage ::: Type usage

infix 6 :::

data Binding usage = Name :@ usage

infix 7 :@


data Type usage
  = Bool
  | Type
  | Constraint usage :-> Type usage
