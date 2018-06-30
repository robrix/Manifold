module Manifold.Proof.Focusing where

import Manifold.Name

newtype Substitution def = Substitution { unSubstitution :: [Replacement def] }

data Replacement def = def ://: Name
infix 9 ://:

data Value
  = Value (Substitution Function) Pattern

data Function
  = FLam [Pattern -> Expr]
  | FId
  | FIdF Name

data Expr
  = EVal Value
  | EApp Function Name Value
  | ECutV Function Value
  | ECutE Function Expr

data Pattern
  = Wildcard
  | PVar Name
  | PUnit
  | PPair Pattern Pattern
  | PInL Pattern
  | PInR Pattern
