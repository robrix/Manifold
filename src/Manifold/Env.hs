module Manifold.Env where

import Manifold.Constraint
import Manifold.Context
import Manifold.Name

type Env addr = Context (Constraint Name addr)
