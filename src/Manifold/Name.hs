module Manifold.Name where

data Name
  = N String
  | I Int
  deriving (Eq, Ord, Show)


class Named n where
  name :: n -> Name
  setName :: Name -> n -> n

instance Named Name where
  name = id
  setName = const

instance Named a => Named (a, b) where
  name = name . fst
  setName n = (,) . setName n . fst <*> snd
