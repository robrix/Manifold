module Manifold.Context where

import Manifold.Name
import Manifold.Type

data Context usage
  = Empty
  | Context usage :> Constraint usage

infixl 5 :>

data Constraint usage = Name ::: Type
