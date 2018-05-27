{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Manifold.Name.Annotated where

import Manifold.Name
import Manifold.Pretty

data Annotated usage = Annotated { annotatedName :: Name, annotatedUsage :: usage }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Named (Annotated usage) where
  name = name . annotatedName
  setName n b = b { annotatedName = n }

instance Pretty usage => Pretty (Annotated usage) where
  prettyPrec d (Annotated name usage) = prettyPrec d name . showChar ' ' . showChar '@' . prettyPrec d usage
