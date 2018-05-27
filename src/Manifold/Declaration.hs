module Manifold.Declaration where

import Manifold.Constraint
import Manifold.Name
import Manifold.Term
import Manifold.Type

data Declaration var def = Declaration
  { declarationConstraint :: Constraint var (Type var)
  , declarationDefinition :: def
  }

declarationName :: Named var => Declaration var def -> Name
declarationName = constraintName . declarationConstraint


data Definition var = Definition
  { definitionPatterns :: [var]
  , definitionBody     :: Term
  }
