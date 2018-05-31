module Manifold.Declaration where

import Manifold.Constraint
import Manifold.Name
import Manifold.Pretty
import Manifold.Term
import Manifold.Type

data Declaration var def
  = Binding (Constraint var (Type var)) def
  deriving (Eq, Ord, Show)

instance (Pretty var, Pretty def) => Pretty (Declaration var def) where
  prettyPrec _ (Binding sig def)
    = prettys sig . showChar '\n'
    . prettys (constraintVar sig) . showChar ' ' . showChar '=' . showChar ' ' . prettys def

declarationSignature :: Declaration var def -> Constraint var (Type var)
declarationSignature (Binding sig _) = sig

declarationName :: Named var => Declaration var def -> Name
declarationName = constraintName . declarationSignature

declarationType :: Declaration var def -> Type var
declarationType = constraintValue . declarationSignature


data Definition var = Definition
  { definitionPatterns :: [var]
  , definitionBody     :: Term var
  }
  deriving (Eq, Ord, Show)
