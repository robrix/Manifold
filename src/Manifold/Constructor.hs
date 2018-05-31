module Manifold.Constructor where

import Manifold.Name
import Manifold.Pretty

newtype Constructor = Constructor Name
  deriving (Eq, Ord, Show)

instance Pretty Constructor where
  prettyPrec d (Constructor n) = prettyPrec d n
