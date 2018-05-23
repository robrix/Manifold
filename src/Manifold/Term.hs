{-# LANGUAGE TypeFamilies #-}
module Manifold.Term where

import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Manifold.Constraint
import Manifold.Expr
import Manifold.Name
import Manifold.Pretty
import Manifold.Type

newtype Term = Term { unTerm :: Expr (Constraint Name Term) Term }
  deriving (Eq, Ord)

instance Show Term where
  showsPrec d = showsUnaryWith showsPrec "Term" d . unSilent . rerep id

type instance Base Term = Expr (Constraint Name Term)

instance Recursive   Term where project = unTerm
instance Corecursive Term where embed   =   Term

instance Pretty Term where
  prettyPrec d = prettyPrec d . unTerm


var :: Name -> Term
var = Term . Var

intro :: Intro (Constraint Name Term) Term Term -> Term
intro = Term . Intro

elim :: Elim Term -> Term
elim = Term . Elim


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


makeLet :: Named var => Constraint Name (Type var) -> Term -> Term -> Term
makeLet (var ::: ty) value body = abs' (var ::: ty) body # value

let' :: Named var => Type var -> Term -> (Term -> Term) -> Term
let' ty value f = makeLet (name ::: ty) value body where (name, body) = bindVariable f


abs' :: Named var => Constraint Name (Type var) -> Term -> Term
abs' constraint body = intro (Abs (rerep name <$> constraint) body)


lam :: Named var => Type var -> (Term -> Term) -> Term
lam ty f = abs' (name ::: ty) body where (name, body) = bindVariable f


(#) :: Term -> Term -> Term
f # a = elim (App f a)

infixl 9 #
