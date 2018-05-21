module Manifold.Binding where

import Manifold.Name

data Binding usage = Binding { bindingName :: Name, bindingUsage :: usage }
  deriving (Eq, Ord, Show)

instance Named (Binding usage) where
  name = name . bindingName
  setName n b = b { bindingName = n }
