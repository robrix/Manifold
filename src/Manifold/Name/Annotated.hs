{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Manifold.Name.Annotated where

import Manifold.Name
import Manifold.Pretty

data Annotated usage = Annotated { bindingName :: Name, bindingUsage :: usage }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Named (Annotated usage) where
  name = name . bindingName
  setName n b = b { bindingName = n }

instance Pretty usage => Pretty (Annotated usage) where
  prettyPrec d (Annotated name usage) = prettyPrec d name . showChar ' ' . showChar '@' . prettyPrec d usage
