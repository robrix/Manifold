module Manifold.Judgement where

import Manifold.Context
import Manifold.Presyntax

data Proposition usage
  = Type usage :=: Type usage
  | Check (Type usage)

data Judgement usage
  = Assumptions usage :- [Proposition usage]

(|-) :: Assumptions usage -> [Proposition usage] -> Judgement usage
(|-) = (:-)

data Assumptions usage
  = Zeroed  (Context usage)
  | Assumed (Context usage)
