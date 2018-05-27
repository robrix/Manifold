module Manifold.Module where

import Data.List
import Manifold.Declaration
import Manifold.Name
import Manifold.Pretty

data Module var def = Module
  { moduleName         :: Name
  , moduleDeclarations :: [Declaration var def]
  }
  deriving (Eq, Ord, Show)

instance (Pretty var, Pretty def) => Pretty (Module var def) where
  prettyPrec _ (Module name decls)
    = showString "module" . showChar ' ' . prettys name . showChar ' ' . showString "where" . showChar '\n'
    . foldr (.) id (intersperse (showChar '\n' . showChar '\n') (map prettys decls))
