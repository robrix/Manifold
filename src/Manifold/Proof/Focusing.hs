{-# LANGUAGE DeriveFoldable #-}
module Manifold.Proof.Focusing where

import Data.Semilattice.Lower
import Manifold.Name

type Substitution def = [Replacement def]

data Replacement def = def ://: Name
  deriving (Eq, Foldable, Ord, Show)

infix 9 ://:

(//) :: def -> Name -> Replacement def
(//) = (://:)

infix 9 //


data Value
  = Value (Substitution Function) Pattern

vf :: Function -> Value
vf f = Value [f // name] (PVar name)
  where name = I 0

vunit :: Value
vunit = Value lowerBound PUnit


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

data Type
  = Type :*: Type
  | Type :+: Type
  | Type :->: Type
  | TUnit

infixl 7 :*:
infixl 6 :+:
infixr 0 :->:
