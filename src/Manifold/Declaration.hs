module Manifold.Declaration where

import Manifold.Constraint
import Manifold.Name
import Manifold.Pretty
import Manifold.Term
import Manifold.Type

data Declaration var def = Declaration
  { declarationSignature  :: Constraint var (Type var)
  , declarationDefinition :: def
  }
  deriving (Eq, Ord, Show)

instance (Pretty var, Pretty def) => Pretty (Declaration var def) where
  prettyPrec _ (Declaration sig def)
    = prettys sig . showChar '\n'
    . prettys (constraintVar sig) . showChar ' ' . showChar '=' . showChar ' ' . prettys def

declarationName :: Named var => Declaration var def -> Name
declarationName = constraintName . declarationSignature

declarationType :: Declaration var def -> Type var
declarationType = constraintValue . declarationSignature


data Definition var = Definition
  { definitionPatterns :: [var]
  , definitionBody     :: Term var
  }
  deriving (Eq, Ord, Show)
