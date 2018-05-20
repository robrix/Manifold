module Manifold.Purpose where

data Purpose
  = Extensional
  | Intensional
  deriving (Eq, Ord, Show)

instance Semigroup Purpose where (<>)   = max
instance Monoid    Purpose where mempty = Extensional
