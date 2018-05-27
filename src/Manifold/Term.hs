{-# LANGUAGE TypeFamilies #-}
module Manifold.Term where

import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Constraint
import Manifold.Expr
import Manifold.Name
import Manifold.Pretty
import Manifold.Type

newtype Term = Term { unTerm :: Expr (Constraint Name (Type Name)) Term }
  deriving (Eq, Ord, Show)

type instance Base Term = Expr (Constraint Name (Type Name))

instance Recursive   Term where project = unTerm
instance Corecursive Term where embed   =   Term

instance Pretty Term where
  prettyPrec d = prettyPrec d . unTerm


var :: Name -> Term
var = Term . Var

intro :: Intro (Constraint Name (Type Name)) Term Term -> Term
intro = Term . Intro

elim :: Elim Term -> Term
elim = Term . Elim


asType :: Term -> Type Name
asType = cata Type


unit :: Term
unit = intro Unit

true :: Term
true = intro (Bool True)

false :: Term
false = intro (Bool False)

iff :: Term -> Term -> Term -> Term
iff c t e = elim (If c t e)


pair :: Term -> Term -> Term
pair a b = intro (Pair a b)

exl :: Term -> Term
exl = elim . ExL

exr :: Term -> Term
exr = elim . ExR


makeLet :: Constraint Name (Type Name) -> Term -> Term -> Term
makeLet (var ::: ty) value body = abs' (var ::: ty) body # value

let' :: Type Name -> Term -> (Term -> Term) -> Term
let' ty value f = makeLet (name ::: ty) value body where (name, body) = bindVariable f


abs' :: Constraint Name (Type Name) -> Term -> Term
abs' constraint body = intro (Abs constraint body)


lam :: Type Name -> (Term -> Term) -> Term
lam ty f = abs' (name ::: ty) body where (name, body) = bindVariable f


(#) :: Term -> Term -> Term
f # a = elim (App f a)

infixl 9 #
