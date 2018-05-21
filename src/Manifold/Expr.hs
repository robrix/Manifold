{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase, TypeFamilies #-}
module Manifold.Expr where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Maybe (fromMaybe)
import Data.Semiring (Unital(..))
import qualified Data.Set as Set
import Manifold.Name
import Manifold.Substitution

data Intro var recur
  = Unit
  | Bool Bool
  | Abs var recur
  | Pair recur recur
  | UnitT
  | BoolT
  | TypeT
  | var :-> recur
  | recur :* recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 0 :->
infixl 7 :*

instance Bifoldable Intro where
  bifoldMap f g = \case
    Unit     -> mempty
    Bool _   -> mempty
    Abs v b  -> f v <> g b
    Pair a b -> g a <> g b
    UnitT    -> mempty
    BoolT    -> mempty
    TypeT    -> mempty
    v :-> b  -> f v <> g b
    a :* b   -> g a <> g b

instance Bifunctor Intro where
  bimap f g = \case
    Unit     -> Unit
    Bool b   -> Bool b
    Abs v b  -> f v `Abs` g b
    Pair a b -> g a `Pair` g b
    UnitT    -> UnitT
    BoolT    -> BoolT
    TypeT    -> TypeT
    v :-> b  -> f v :-> g b
    a :* b   -> g a :* g b

data Elim recur
  = ExL recur
  | ExR recur
  | App recur recur
  | If recur recur recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Expr var recur
  = Var Name
  | Intro (Intro var recur)
  | Elim (Elim recur)
  deriving (Eq, Foldable, Functor, Ord, Traversable)

instance Bifoldable Expr where
  bifoldMap f g = \case
    Var _   -> mempty
    Intro i -> bifoldMap f g i
    Elim e  -> foldMap g e

instance Bifunctor Expr where
  bimap f g = \case
    Var n   -> Var n
    Intro i -> Intro (bimap f g i)
    Elim e  -> Elim (fmap g e)

instance (Show var, Show recur) => Show (Expr var recur) where
  showsPrec d = \case
    Var n   -> showsUnaryWith showsPrec "Var" d n
    Intro i -> showsPrec d i
    Elim e  -> showsPrec d e


newtype Type usage = Type { unType :: Expr (Constraint usage (Type usage)) (Type usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Type usage) where
  showsPrec d = showsUnaryWith showsPrec "Type" d . cata Silent

type instance Base (Type usage) = Expr (Constraint usage (Type usage))

instance Recursive   (Type usage) where project = unType
instance Corecursive (Type usage) where embed   =   Type

instance Substitutable (Type usage) where
  apply subst ty = case unType ty of
    Var name                                -> fromMaybe (tvar name) (lookupSubst name subst)
    Intro ((name, usage) ::: ty :-> body)   -> tintro ((name, usage) ::: apply subst ty :-> apply (deleteSubst name subst) body)
    Intro (Abs ((name, usage) ::: ty) body) -> tintro (Abs ((name, usage) ::: apply subst ty) (apply (deleteSubst name subst) body))
    Intro i                                 -> tintro (apply subst <$> i)
    Elim e                                  -> telim (apply subst <$> e)

tvar :: Name -> Type usage
tvar = Type . Var

tintro :: Intro (Constraint usage (Type usage)) (Type usage) -> Type usage
tintro = Type . Intro

telim :: Elim (Type usage) -> Type usage
telim = Type . Elim


typeT :: Type usage
typeT = tintro TypeT

unitT :: Type usage
unitT = tintro UnitT

boolT :: Type usage
boolT = tintro BoolT


(.->) :: Constraint usage (Type usage) -> Type usage -> Type usage
constraint .-> ty = tintro (constraint :-> ty)

infixr 0 .->

(.*) :: Type usage -> Type usage -> Type usage
a .* b = tintro (a :* b)

infixl 7 .*


newtype Term usage = Term { unTerm :: Expr (Constraint usage (Term usage)) (Term usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Term usage) where
  showsPrec d = showsUnaryWith showsPrec "Term" d . cata Silent

type instance Base (Term usage) = Expr (Constraint usage (Term usage))

instance Recursive   (Term usage) where project = unTerm
instance Corecursive (Term usage) where embed   =   Term

var :: Name -> Term usage
var = Term . Var

intro :: Intro (Constraint usage (Term usage)) (Term usage) -> Term usage
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
makeLet ((name, usage) ::: ty) value body = abs' ((name, usage) ::: ty) body # value

let' :: Unital usage => Type usage -> Term usage -> (Term usage -> Term usage) -> Term usage
let' ty value f = makeLet ((name, one) ::: ty) value body where (name, body) = bindVariable f


abs' :: Constraint usage (Type usage) -> Term usage -> Term usage
abs' constraint body = intro (Abs (rerep <$> constraint) body)

rerep :: (Recursive t1, Base t1 ~ Expr (Constraint usage t1), Corecursive t2, Base t2 ~ Expr (Constraint usage t2))
      => t1
      -> t2
rerep = cata (embed . first (fmap rerep))

lam :: Unital usage => Type usage -> (Term usage -> Term usage) -> Term usage
lam ty f = abs' ((name, one) ::: ty) body
  where (name, body) = bindVariable f

bindVariable :: (Term usage -> Term usage) -> (Name, Term usage)
bindVariable f = (name, body)
  where name = maybe (I 0) prime (maxBV body)
        body = f (var name)
        prime (I i) = I (succ i)
        prime (N s) = N (s <> "ʹ")

maxBV :: (Recursive t, Base t ~ Expr (Constraint usage t)) => t -> Maybe Name
maxBV = cata $ \case
  Intro ((name, _) ::: ty :-> _) -> max (Just name) (maxBV ty)
  Intro (Abs ((name, _) ::: ty) _) -> max (Just name) (maxBV ty)
  other -> foldr max Nothing other


freeVariables :: (Recursive t, Base t ~ Expr (Constraint usage t)) => t -> Set.Set Name
freeVariables = cata $ \case
  Var name -> Set.singleton name
  Intro ((name, _) ::: ty :-> body) -> freeVariables ty <> Set.delete name body
  Intro (Abs ((name, _) ::: ty) body) -> freeVariables ty <> Set.delete name body
  other -> fold other


(#) :: Term usage -> Term usage -> Term usage
f # a = elim (App f a)

infixl 9 #


data Constraint usage recur = Binding usage ::: recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable Constraint where
  bifoldMap f g ((_, usage) ::: ty) = f usage <> g ty

instance Bifunctor Constraint where
  bimap f g ((name, usage) ::: ty) = (name, f usage) ::: g ty

infix 5 :::

constraintName :: Constraint usage recur -> Name
constraintName ((name, _) ::: _) = name

constraintValue :: Constraint usage recur -> recur
constraintValue (_ ::: ty) = ty


type Binding usage = (Name, usage)


instance Foldable Type where
  foldMap f = bifoldMap (bifoldMap f (foldMap f)) (foldMap f) . unType

instance Functor Type where
  fmap f = Type . bimap (bimap f (fmap f)) (fmap f) . unType


instance Foldable Term where
  foldMap f = bifoldMap (bifoldMap f (foldMap f)) (foldMap f) . unTerm

instance Functor Term where
  fmap f = Term . bimap (bimap f (fmap f)) (fmap f) . unTerm


newtype Silent usage = Silent { unSilent :: Expr usage (Silent usage) }

instance Show usage => Show (Silent usage) where
  showsPrec d = showsPrec d . unSilent
