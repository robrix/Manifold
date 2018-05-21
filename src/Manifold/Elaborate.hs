module Manifold.Elaborate where

import Manifold.Constraint
import Manifold.Expr
import Manifold.Type

data Elab usage = Elab { elabExpr :: Expr (Constraint usage (Elab usage)) (Elab usage), elabType :: Type usage }
  deriving (Eq, Ord)
