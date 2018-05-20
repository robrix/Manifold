{-# LANGUAGE DeriveFoldable, GeneralizedNewtypeDeriving #-}
module Manifold.Substitution where

import Data.Maybe (isNothing)
import Manifold.Name

newtype Substitution term = Substitution { getSubstitution :: [(Name, term)] }
  deriving (Eq, Foldable, Ord, Show)

instance Semigroup (Substitution term) where
  Substitution as <> Substitution bs = Substitution (as <> filter (isNothing . flip lookup as . fst) bs)

instance Monoid (Substitution term) where
  mempty = Substitution []
