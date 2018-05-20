module Manifold.Context where

import Manifold.Type

data Context usage
  = Empty
  | Context usage :> Constraint usage

infixl 5 :>
