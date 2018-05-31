{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase #-}
module Manifold.Term.Elim where

import Data.Functor.Classes
import Manifold.Pretty

data Elim recur
  = ExL recur
  | ExR recur
  | App recur recur
  | If recur recur recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Pretty recur => Pretty (Elim recur) where
  prettyPrec d = \case
    ExL a -> showsUnaryWith prettyPrec "exl" d a
    ExR a -> showsUnaryWith prettyPrec "exr" d a
    App f a -> showParen (d > 10) $ prettyPrec 10 f . showChar ' ' . prettyPrec 11 a
    If c t e -> showParen (d > (-1))
      $ showString "if"   . showSpace     (prettyPrec 0    c)
      . showString "then" . showSpace     (prettyPrec 0    t)
      . showString "then" . showChar ' ' . prettyPrec (-1) e
