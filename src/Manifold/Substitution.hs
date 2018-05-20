{-# LANGUAGE DeriveFoldable, GeneralizedNewtypeDeriving #-}
module Manifold.Substitution where

import qualified Data.Map as Map
import Manifold.Name

newtype Substitution term = Substitution { getSubstitution :: Map.Map Name term }
  deriving (Eq, Foldable, Ord, Show)


lookupSubst :: Name -> Substitution term -> Maybe term
lookupSubst name = Map.lookup name . getSubstitution

deleteSubst :: Name -> Substitution term -> Substitution term
deleteSubst name = Substitution . Map.delete name . getSubstitution


instance Substitutable term => Semigroup (Substitution term) where
  a <> b = Substitution (Map.unionWith const (Map.map (apply b) (getSubstitution a)) (getSubstitution b))

instance Substitutable term => Monoid (Substitution term) where
  mempty = Substitution Map.empty


class Substitutable s where
  apply :: Substitution s -> s -> s
