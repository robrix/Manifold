{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase, TypeFamilies #-}
module Manifold.Expr where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Manifold.Name
import Manifold.Substitution

data Intro var scope recur
  = Unit
  | Bool Bool
  | Abs var scope
  | Pair recur recur
  | UnitT
  | BoolT
  | TypeT
  | var :-> scope
  | recur :* recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 0 :->
infixl 7 :*

class Trifoldable t where
  trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> t a b c -> m

class Trifunctor t where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> t a b c -> t a' b' c'

instance Trifoldable Intro where
  trifoldMap f g h = \case
    Unit     -> mempty
    Bool _   -> mempty
    Abs v b  -> f v <> g b
    Pair a b -> h a <> h b
    UnitT    -> mempty
    BoolT    -> mempty
    TypeT    -> mempty
    v :-> b  -> f v <> g b
    a :* b   -> h a <> h b

instance Trifunctor Intro where
  trimap f g h = \case
    Unit     -> Unit
    Bool b   -> Bool b
    Abs v b  -> f v `Abs` g b
    Pair a b -> h a `Pair` h b
    UnitT    -> UnitT
    BoolT    -> BoolT
    TypeT    -> TypeT
    v :-> b  -> f v :-> g b
    a :* b   -> h a :* h b

data Elim recur
  = ExL recur
  | ExR recur
  | App recur recur
  | If recur recur recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data Expr var recur
  = Var Name
  | Intro (Intro var recur recur)
  | Elim (Elim recur)
  deriving (Eq, Ord)

instance Bifoldable Expr where
  bifoldMap f g = \case
    Var _   -> mempty
    Intro i -> trifoldMap f g g i
    Elim e  -> foldMap g e

instance Foldable (Expr var) where
  foldMap = bifoldMap (const mempty)

instance Bifunctor Expr where
  bimap f g = \case
    Var n   -> Var n
    Intro i -> Intro (trimap f g g i)
    Elim e  -> Elim (fmap g e)

instance Functor (Expr var) where
  fmap = bimap id

instance (Show var, Show recur) => Show (Expr var recur) where
  showsPrec d = \case
    Var n   -> showsUnaryWith showsPrec "Var" d n
    Intro i -> showsPrec d i
    Elim e  -> showsPrec d e


newtype Type usage = Type { unType :: Expr (Constraint usage (Type usage)) (Type usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Type usage) where
  showsPrec d = showsUnaryWith showsPrec "Type" d . unSilent . rerep

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

tintro :: Intro (Constraint usage (Type usage)) (Type usage) (Type usage) -> Type usage
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



rerep :: (Recursive t1, Base t1 ~ f (Constraint usage t1), Corecursive t2, Base t2 ~ f (Constraint usage t2), Bifunctor f)
      => t1
      -> t2
rerep = cata (embed . first (fmap rerep))


bindVariable :: (Corecursive t, Recursive t, Base t ~ Expr (Constraint usage t)) => (t -> t) -> (Name, t)
bindVariable f = (name, body)
  where name = maybe (I 0) prime (maxBV body)
        body = f (embed (Var name))
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




newtype Silent usage = Silent { unSilent :: Expr (Constraint usage (Silent usage)) (Silent usage) }

type instance Base (Silent usage) = Expr (Constraint usage (Silent usage))

instance Recursive   (Silent usage) where project = unSilent
instance Corecursive (Silent usage) where embed   =   Silent

instance Show usage => Show (Silent usage) where
  showsPrec d = showsPrec d . unSilent
