{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Manifold.Expr.Elim where

data Elim recur
  = ExL recur
  | ExR recur
  | App recur recur
  | If recur recur recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)
