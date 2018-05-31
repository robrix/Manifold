module Manifold.Pattern where

import Manifold.Name

data Pattern
  = Wildcard
  | Variable Name
  | Constructor Name [Pattern]
  deriving (Eq, Ord, Show)
