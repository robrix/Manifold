module Manifold.Name where

import Manifold.Pretty

data Name
  = N String
  | Q String Name
  | I Int
  deriving (Eq, Ord, Show)


class Named n where
  name :: n -> Name
  setName :: Name -> n -> n

instance Named Name where
  name = id
  setName = const


instance Pretty Name where
  prettyPrec _ (N s) = showString s
  prettyPrec _ (Q s n) = showString s . showChar '.' . prettys n
  prettyPrec _ (I n) = (if n < 0 then showChar '_' else id) . showString (replicate (succ i) (alphabet !! r))
    where alphabet = ['a'..'z']
          (i, r) = abs n `divMod` length alphabet
