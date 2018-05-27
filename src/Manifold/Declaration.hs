module Manifold.Declaration where

import Manifold.Term
import Manifold.Type

data Declaration var
  = Declaration (Signature var) (Definition var)

data Signature var = Signature
  { signatureName :: var
  , signatureType :: Type var
  }

data Definition var = Definition
  { definitionName     :: var
  , definitionPatterns :: [var]
  , definitionBody     :: Term
  }
