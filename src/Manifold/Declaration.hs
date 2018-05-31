module Manifold.Declaration where

import Manifold.Constraint
import Manifold.Name
import Manifold.Pretty
import Manifold.Term
import Manifold.Type

data Declaration var def
  = Binding (Constraint var (Type var)) def
  | Datatype (Constraint var (Type var)) [Constraint var (Type var)]
  deriving (Eq, Ord, Show)

instance (Pretty var, Pretty def) => Pretty (Declaration var def) where
  prettyPrec _ (Binding sig def)
    = prettys sig . showChar '\n'
    . prettys (constraintVar sig) . showChar ' ' . showChar '=' . showChar ' ' . prettys def
  prettyPrec _ (Datatype sig constructors)
    = showString "data" . showChar ' ' . prettys sig . showChar ' ' . showString "where" . showChar '\n'
    . foldr (.) id (map (\ c -> showChar ' ' . showChar ' ' . prettys c . showChar '\n') constructors)

declarationSignature :: Declaration var def -> Constraint var (Type var)
declarationSignature (Binding sig _) = sig
declarationSignature (Datatype sig _) = sig

declarationSignatures :: Declaration var def -> [Constraint var (Type var)]
declarationSignatures (Binding sig _) = [sig]
declarationSignatures (Datatype sig constructors) = sig : constructors

declarationName :: Named var => Declaration var def -> Name
declarationName = constraintName . declarationSignature

declarationType :: Declaration var def -> Type var
declarationType = constraintValue . declarationSignature


data Definition var = Definition
  { definitionPatterns :: [var]
  , definitionBody     :: Term var
  }
  deriving (Eq, Ord, Show)
