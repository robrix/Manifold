module Manifold.Module where

import Manifold.Declaration
import Manifold.Name

data Module var def = Module
  { moduleName         :: Name
  , moduleDeclarations :: [Declaration var def]
  }
