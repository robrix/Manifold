module Manifold.Pattern where

import Manifold.Name

data Pattern
  = Wildcard
  | Variable Name
  deriving (Eq, Ord, Show)
