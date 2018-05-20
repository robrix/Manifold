module Manifold.Type where

import Manifold.Name

data Constraint usage = Binding usage ::: Type

infix 6 :::

data Binding usage = Name :@ usage

infix 7 :@


data Type
  = Bool
  | Type
