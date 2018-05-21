{-# LANGUAGE TypeFamilies #-}
module Manifold.Term where

import Data.Bifoldable
import Data.Bifunctor
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Semiring (Unital(..))
import Manifold.Binding
import Manifold.Constraint
import Manifold.Expr
import Manifold.Name
import Manifold.Type

newtype Term usage = Term { unTerm :: Expr (Constraint usage (Term usage)) (Term usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Term usage) where
  showsPrec d = showsUnaryWith showsPrec "Term" d . unSilent . rerep

type instance Base (Term usage) = Expr (Constraint usage (Term usage))

instance Recursive   (Term usage) where project = unTerm
instance Corecursive (Term usage) where embed   =   Term

var :: Name -> Term usage
var = Term . Var

intro :: Intro (Constraint usage (Term usage)) (Term usage) (Term usage) -> Term usage
intro = Term . Intro

elim :: Elim (Term usage) -> Term usage
elim = Term . Elim


unit :: Term usage
unit = intro Unit

true :: Term usage
true = intro (Bool True)

false :: Term usage
false = intro (Bool False)

iff :: Term usage -> Term usage -> Term usage -> Term usage
iff c t e = elim (If c t e)


pair :: Term usage -> Term usage -> Term usage
pair a b = intro (Pair a b)

exl :: Term usage -> Term usage
exl = elim . ExL

exr :: Term usage -> Term usage
exr = elim . ExR


makeLet :: Constraint usage (Type usage) -> Term usage -> Term usage -> Term usage
makeLet (var ::: ty) value body = abs' (var ::: ty) body # value

let' :: Unital usage => Type usage -> Term usage -> (Term usage -> Term usage) -> Term usage
let' ty value f = makeLet (Binding name one ::: ty) value body where (name, body) = bindVariable f


abs' :: Constraint usage (Type usage) -> Term usage -> Term usage
abs' constraint body = intro (Abs (rerep <$> constraint) body)


lam :: Unital usage => Type usage -> (Term usage -> Term usage) -> Term usage
lam ty f = abs' (Binding name one ::: ty) body
  where (name, body) = bindVariable f


(#) :: Term usage -> Term usage -> Term usage
f # a = elim (App f a)

infixl 9 #


instance Foldable Term where
  foldMap f = bifoldMap (bifoldMap f (foldMap f)) (foldMap f) . unTerm

instance Functor Term where
  fmap f = Term . bimap (bimap f (fmap f)) (fmap f) . unTerm
