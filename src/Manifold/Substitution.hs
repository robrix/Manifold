{-# LANGUAGE DeriveFoldable, GeneralizedNewtypeDeriving #-}
module Manifold.Substitution where

import Manifold.Name

newtype Substitution term = Substitution { getSubstitution :: [(Name, term)] }
  deriving (Eq, Foldable, Ord, Show)
