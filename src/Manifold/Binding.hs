{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Manifold.Binding where

import Manifold.Name

data Binding usage = Binding { bindingName :: Name, bindingUsage :: usage }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Named (Binding usage) where
  name = name . bindingName
  setName n b = b { bindingName = n }
