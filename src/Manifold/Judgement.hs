module Manifold.Judgement where

import Data.Semiring (zero)
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


typing :: Monoid usage => Judgement usage -> Either (Judgement usage) [Judgement usage]
typing judgement = case judgement of
  Zeroed gamma :- [ Check (Type Bool) ] ->
    Right [ Zeroed gamma |- [] ]
  Zeroed gamma :- [ Check (Type (((x, _) ::: _S) :-> _T)) ] ->
    Right [ Zeroed gamma |- [ Check _S ]
          , Zeroed (gamma :> ((x, zero) ::: _S)) |- [ Check _T ]
          ]
  other -> Left other
