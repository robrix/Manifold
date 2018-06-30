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

instance Named var => Named (Declaration var def) where
  name = name . declarationSignature
  setName name (Binding sig def) = Binding (setName name sig) def
  setName name (Datatype sig cons) = Datatype (setName name sig) cons

instance (Pretty var, Pretty def) => Pretty (Declaration var def) where
  prettyPrec _ (Binding sig def)
    = vsep [ pretty sig, pretty (constraintVar sig) <+> equals <+> pretty def ]
  prettyPrec _ (Datatype sig [])
    = prettyString "data" <+> pretty sig
  prettyPrec _ (Datatype sig constructors)
    = nest 2 . vsep
      $ prettyString "data" <+> pretty sig <+> prettyString "where"
      : map pretty constructors


declarationSignature :: Declaration var def -> Constraint var (Type var)
declarationSignature (Binding sig _) = sig
declarationSignature (Datatype sig _) = sig

declarationSignatures :: Declaration var def -> [Constraint var (Type var)]
declarationSignatures (Binding sig _) = [sig]
declarationSignatures (Datatype sig constructors) = sig : constructors

declarationType :: Declaration var def -> Type var
declarationType = constraintValue . declarationSignature


data Definition var = [var] := Term var
  deriving (Eq, Ord, Show)
