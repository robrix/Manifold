{-# LANGUAGE LambdaCase, TypeFamilies #-}
module Manifold.Pattern where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Name
import Manifold.Pretty

newtype Pattern var = Pattern { unPattern :: PExpr var (Pattern var) }
  deriving (Eq, Ord, Show)

type instance Base (Pattern var) = PExpr var

instance Recursive   (Pattern var) where project = unPattern
instance Corecursive (Pattern var) where embed   =   Pattern

instance Pretty var => Pretty (Pattern var) where
  prettyPrec d = prettyPrec d . unPattern


wildcard :: Pattern var
wildcard = Pattern Wildcard

pvar :: var -> Pattern var
pvar = Pattern . Variable

constructor :: Name -> [Pattern var] -> Pattern var
constructor name = Pattern . Constructor name


pair :: Pattern var -> Pattern var -> Pattern var
pair a b = Pattern $ Constructor (N "Pair") [a, b]

unit :: Pattern var
unit = Pattern $ Constructor (N "Unit") []


data PExpr var recur
  = Wildcard
  | Variable var
  | Constructor Name [recur]
  deriving (Eq, Ord, Show)

instance Bifoldable PExpr where
  bifoldMap f g = \case
    Wildcard         -> mempty
    Variable v       -> f v
    Constructor _ ps -> foldMap g ps

instance Foldable (PExpr var) where
  foldMap = bifoldMap (const mempty)

instance Bifunctor PExpr where
  bimap f g = \case
    Wildcard         -> Wildcard
    Variable v       -> Variable (f v)
    Constructor n ps -> Constructor n (fmap g ps)

instance Functor (PExpr var) where
  fmap = bimap id

instance (Pretty var, Pretty recur) => Pretty (PExpr var recur) where
  prettyPrec d = \case
    Wildcard                  -> prettyString "_"
    Variable var              -> prettyPrec d var
    Constructor name patterns -> prettyParen (d > 10) $ prettyPrec 10 name <> foldMap ((space <>) . prettyPrec 11) patterns
