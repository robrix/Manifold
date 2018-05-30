module Manifold.Constructor where

import Manifold.Pretty

newtype Constructor = Constructor String
  deriving (Eq, Ord, Show)

instance Pretty Constructor where
  prettyPrec _ (Constructor s) = showString s
