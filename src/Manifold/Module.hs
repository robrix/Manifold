module Manifold.Module where

import Data.List
import qualified Data.Map as Map
import Manifold.Declaration
import Manifold.Name
import Manifold.Pretty

data Module var def = Module
  { moduleName         :: Name
  , moduleImports      :: [Name]
  , moduleDeclarations :: [Declaration var def]
  }
  deriving (Eq, Ord, Show)

instance (Pretty var, Pretty def) => Pretty (Module var def) where
  prettyPrec _ (Module name imports decls)
    = showString "module" . showChar ' ' . prettys name . showChar ' ' . showString "where" . showChar '\n' . showChar '\n'
    . foldr (.) id (intersperse (showChar '\n') (map (fmap (showString "import" . showChar ' ') . prettys) imports)) . showChar '\n' . showChar '\n'
    . foldr (.) id (intersperse (showChar '\n' . showChar '\n') (map prettys decls))


newtype ModuleTable var def = ModuleTable { unModuleTable :: Map.Map Name (Module var def) }
  deriving (Eq, Ord, Show)
