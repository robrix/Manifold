module Manifold.Context where

import Manifold.Name
import Manifold.Type

data Context
  = Empty
  | Context :> Constraint

data Constraint = Name ::: Type
