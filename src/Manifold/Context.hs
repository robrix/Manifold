module Manifold.Context where

import Manifold.Presyntax

data Context usage
  = Empty
  | Context usage :> Constraint usage

infixl 5 :>
