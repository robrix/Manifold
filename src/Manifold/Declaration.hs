module Manifold.Declaration where

import Manifold.Term
import Manifold.Type

data Declaration var = Declaration
  { declarationSignature :: Signature var
  , declarationDefinitions :: [Definition var]
  }


data Signature var = Signature
  { signatureVar  :: var
  , signatureType :: Type var
  }

data Definition var = Definition
  { definitionVar      :: var
  , definitionPatterns :: [var]
  , definitionBody     :: Term
  }
