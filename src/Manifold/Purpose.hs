module Manifold.Purpose where

import Data.Semiring (Semiring(..))

data Purpose
  = Extensional
  | Intensional
  deriving (Eq, Ord, Show)

instance Semigroup Purpose where (<>)   = max
instance Monoid    Purpose where mempty = Extensional
instance Semiring  Purpose where (><) = min
