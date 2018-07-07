{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase, OverloadedStrings #-}
module Manifold.Term.Elim where

import Manifold.Name
import Manifold.Pattern
import Manifold.Pretty

data Elim recur
  = App recur recur
  | Case recur [(Pattern Name, recur)]
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Pretty recur => Pretty (Elim recur) where
  prettyPrec d = \case
    App f a -> prettyParen (d > 10) $ prettyPrec 10 f <+> prettyPrec 11 a
    Case subject branches -> prettyParen (d > 11) . nest 2 . group
      $   prettyString "case" <+> prettyPrec 0 subject <+> prettyString "of"
      <+> align (encloseSep open close sep (map (uncurry showBranch) branches))
      where showBranch pattern body = pretty pattern <+> prettyString "->" <+> pretty body
            open = flatAlt "" "{ "
            close = flatAlt "" " }"
            sep = flatAlt "" " ; "
