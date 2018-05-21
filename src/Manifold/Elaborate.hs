module Manifold.Elaborate where

import Manifold.Constraint
import Manifold.Expr
import Manifold.Type

data Elab var = Elab { elabExpr :: Expr (Constraint var (Elab var)) (Elab var), elabType :: Type var }
  deriving (Eq, Ord)
