module Manifold.Judgement where

import Manifold.Context
import Manifold.Presyntax

data Proposition usage
  = Type usage :=: Type usage

data Judgement usage
  = Context usage :- [Proposition usage]
