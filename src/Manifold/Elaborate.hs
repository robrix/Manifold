module Manifold.Elaborate where

import Manifold.Constraint
import Manifold.Expr

newtype Elab usage = Elab { unElab :: Expr (Constraint usage (Elab usage)) (Elab usage) }
  deriving (Eq, Ord)
