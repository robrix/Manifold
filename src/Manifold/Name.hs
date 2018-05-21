module Manifold.Name where

data Name
  = N String
  | I Int
  deriving (Eq, Ord, Show)


class Named n where
  name :: n -> Name

instance Named Name where
  name = id
