{-# LANGUAGE DeriveFoldable #-}
module Manifold.Proof.Focusing where

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
  | VFn Function
  | VUnit
  | VPair Value Value
  | VInL Value
  | VInR Value

vfalse, vtrue :: Value
vfalse = VInL VUnit
vtrue = VInR VUnit


data Function
  = FLam (Pattern -> Expr)
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

pfalse, ptrue :: Pattern
pfalse = PInL PUnit
ptrue = PInR PUnit


data Type
  = Type :*: Type
  | Type :+: Type
  | Type :->: Type
  | TUnit

infixl 7 :*:
infixl 6 :+:
infixr 0 :->:

tbool :: Type
tbool = TUnit :+: TUnit
