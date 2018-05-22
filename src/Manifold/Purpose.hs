module Manifold.Purpose where

import Data.Semiring (Semiring(..), Unital(..), zero)

data Purpose
  = Extensional -- ^ Used for type formation.
  | Intensional -- ^ Used for computation.
  deriving (Eq, Ord, Show)

instance Semigroup Purpose where (<>)   = max
instance Monoid    Purpose where mempty = Extensional
instance Semiring  Purpose where (><)   = min
instance Unital    Purpose where one    = Intensional

fromUsage :: (Eq usage, Monoid usage) => usage -> Purpose
fromUsage usage
  | usage == zero = Extensional
  | otherwise     = Intensional

interpretPurpose :: (Monoid usage, Unital usage) => Purpose -> usage
interpretPurpose Extensional = zero
interpretPurpose Intensional = one
