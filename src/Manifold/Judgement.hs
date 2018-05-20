module Manifold.Judgement where

import Manifold.Presyntax

data Proposition usage
  = Type usage :=: Type usage
