module Manifold.Erasure where

import Data.Semiring (Semiring(..), Unital(..))

data Erasure
  = Erased
  | Present
  deriving (Eq, Ord, Show)

instance Semigroup Erasure where (<>)   = max
instance Monoid    Erasure where mempty = Erased
instance Semiring  Erasure where (><)   = min
instance Unital    Erasure where one    = Present
