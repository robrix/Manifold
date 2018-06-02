{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Term.Elim where

import Data.Foldable (fold)
import Data.List (intersperse)
import Manifold.Pattern
import Manifold.Pretty

data Elim recur
  = ExL recur
  | ExR recur
  | App recur recur
  | If recur recur recur
  | Case recur [(Pattern, recur)]
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Pretty recur => Pretty (Elim recur) where
  prettyPrec d = \case
    ExL a -> prettyParen (d > 10) $ prettyString "exl" <+> prettyPrec 11 a
    ExR a -> prettyParen (d > 10) $ prettyString "exr" <+> prettyPrec 11 a
    App f a -> prettyParen (d > 10) $ prettyPrec 10 f <+> prettyPrec 11 a
    If c t e -> prettyParen (d > (-1)) . align . sep $
      [ prettyString "if"   <+> prettyPrec 0    c
      , prettyString "then" <+> prettyPrec 0    t
      , prettyString "then" <+> prettyPrec (-1) e
      ]
    Case subject branches -> prettyParen (d > 11) . nest 2 . vsep $
      [ prettyString "case" <+> prettyPrec 0 subject <+> prettyString "of"
      , braces (fold (intersperse semi (map (uncurry showBranch) branches)))
      ]
      where showBranch pattern body = pretty pattern <+> prettyString "->" <+> pretty body
