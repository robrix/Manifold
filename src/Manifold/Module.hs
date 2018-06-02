{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Manifold.Module where

import Control.Monad ((<=<))
import qualified Data.Map as Map
import Data.Semilattice.Lower
import Manifold.Constraint
import Manifold.Declaration
import Manifold.Name
import Manifold.Pretty
import Manifold.Type

data Module var def = Module
  { moduleName         :: Name
  , moduleImports      :: [Name]
  , moduleDeclarations :: [Declaration var def]
  }
  deriving (Eq, Ord, Show)

moduleExports :: Module var def -> [Constraint var (Type var)]
moduleExports = declarationSignatures <=< moduleDeclarations

instance (Pretty var, Pretty def) => Pretty (Module var def) where
  prettyPrec _ (Module name imports decls)
    = align $ vvsep
      [ [ prettyString "module" <+> pretty name <+> prettyString "where" ]
      , map ((prettyString "import" <+>) . pretty) imports
      , [ vvsep (map (pure . pretty) decls) ]
      ]
    where vvsep :: [[Doc ann]] -> Doc ann
          vvsep = vsep . (>>= each)
            where each x | null x    = []
                         | otherwise = [ vsep x <> line ]


newtype ModuleTable var def = ModuleTable { unModuleTable :: Map.Map Name (Module var def) }
  deriving (Eq, Lower, Ord, Show)

fromModules :: [Module var def] -> ModuleTable var def
fromModules = ModuleTable . Map.fromList . map ((,) . moduleName <*> id)

insert :: Module var def -> ModuleTable var def -> ModuleTable var def
insert m@(Module name _ _) = ModuleTable . Map.insert name m . unModuleTable

lookup :: Name -> ModuleTable var def -> Maybe (Module var def)
lookup name = Map.lookup name . unModuleTable
