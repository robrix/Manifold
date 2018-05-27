module Manifold.Declaration where

import Manifold.Name
import Manifold.Term
import Manifold.Type

data Declaration var = Declaration
  { declarationSignature :: Signature var
  , declarationDefinitions :: [Definition var]
  }

declarationName :: Named var => Declaration var -> Name
declarationName = name . signatureVar . declarationSignature


data Signature var = Signature
  { signatureVar  :: var
  , signatureType :: Type var
  }

data Definition var = Definition
  { definitionVar      :: var
  , definitionPatterns :: [var]
  , definitionBody     :: Term
  }
