module Manifold.Purpose where

import Data.Semiring (Semiring(..), Unital(..))

data Purpose
  = Extensional
  | Intensional
  deriving (Eq, Ord, Show)

instance Semigroup Purpose where (<>)   = max
instance Monoid    Purpose where mempty = Extensional
instance Semiring  Purpose where (><)   = min
instance Unital    Purpose where one    = Intensional
