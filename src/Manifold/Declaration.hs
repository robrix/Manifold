module Manifold.Declaration where

import Manifold.Constraint
import Manifold.Name
import Manifold.Term
import Manifold.Type

data Declaration var def = Declaration
  { declarationSignature  :: Constraint var (Type var)
  , declarationDefinition :: def
  }

declarationName :: Named var => Declaration var def -> Name
declarationName = constraintName . declarationSignature


data Definition var = Definition
  { definitionPatterns :: [var]
  , definitionBody     :: Term
  }
