module Manifold.Judgement where

import Manifold.Context
import Manifold.Presyntax

data Proposition usage
  = Type usage :=: Type usage

data Judgement usage
  = Assumptions usage :- [Proposition usage]

data Assumptions usage
  = Zeroed  (Context usage)
  | Assumed (Context usage)
