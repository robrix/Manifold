{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, LambdaCase, TypeFamilies #-}
module Manifold.Expr where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable (fold)
import Data.Functor.Classes (showsUnaryWith)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import Data.Maybe (fromMaybe)
import Data.Semiring (Unital(..))
import qualified Data.Set as Set
import Manifold.Name
import Manifold.Substitution

data Expr usage recur
  = Unit
  | UnitT
  | BoolT
  | Bool Bool
  | TypeT
  | Var Name
  | Constraint usage recur :-> recur
  | Abs (Constraint usage recur) recur
  | App recur recur
  | If recur recur recur
  | recur :* recur
  | Pair recur recur
  | ExL recur
  | ExR recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 0 :->
infixl 7 :*


newtype Type usage = Type { unType :: Expr usage (Type usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Type usage) where
  showsPrec d = showsUnaryWith showsPrec "Type" d . cata Silent

type instance Base (Type usage) = Expr usage

instance Recursive   (Type usage) where project = unType
instance Corecursive (Type usage) where embed   =   Type

instance Substitutable (Type usage) where
  apply subst ty = case unType ty of
    Unit                            -> Type Unit
    UnitT                           -> Type UnitT
    BoolT                           -> Type BoolT
    Bool b                          -> Type (Bool b)
    TypeT                           -> Type TypeT
    Var name                        -> fromMaybe (Type (Var name)) (lookupSubst name subst)
    (name, usage) ::: ty :-> body   -> Type ((name, usage) ::: apply subst ty :-> apply (deleteSubst name subst) body)
    Abs ((name, usage) ::: ty) body -> Type (Abs ((name, usage) ::: apply subst ty) (apply (deleteSubst name subst) body))
    App f a                         -> Type (App (apply subst f) (apply subst a))
    If c t e                        -> Type (If (apply subst c) (apply subst t) (apply subst e))
    a :* b                          -> Type (apply subst a :* apply subst b)
    Pair a b                        -> Type (Pair (apply subst a) (apply subst b))
    ExL a                           -> Type (ExL (apply subst a))
    ExR a                           -> Type (ExR (apply subst a))


typeT :: Type usage
typeT = Type TypeT

unitT :: Type usage
unitT = Type UnitT

boolT :: Type usage
boolT = Type BoolT


(.->) :: Constraint usage (Type usage) -> Type usage -> Type usage
constraint .-> ty = Type (constraint :-> ty)

(.*) :: Type usage -> Type usage -> Type usage
a .* b = Type (a :* b)


tvar :: Name -> Type usage
tvar = Type . Var


newtype Term usage = Term { unTerm :: Expr usage (Term usage) }
  deriving (Eq, Ord)

instance Show usage => Show (Term usage) where
  showsPrec d = showsUnaryWith showsPrec "Term" d . cata Silent

type instance Base (Term usage) = Expr usage

instance Recursive   (Term usage) where project = unTerm
instance Corecursive (Term usage) where embed   =   Term


unit :: Term usage
unit = Term Unit

true :: Term usage
true = Term (Bool True)

false :: Term usage
false = Term (Bool False)

iff :: Term usage -> Term usage -> Term usage -> Term usage
iff c t e = Term (If c t e)


pair :: Term usage -> Term usage -> Term usage
pair a b = Term (Pair a b)

exl :: Term usage -> Term usage
exl = Term . ExL

exr :: Term usage -> Term usage
exr = Term . ExR


var :: Name -> Term usage
var = Term . Var


makeLet :: Constraint usage (Type usage) -> Term usage -> Term usage -> Term usage
makeLet ((name, usage) ::: ty) value body = abs' ((name, usage) ::: ty) body # value

let' :: Unital usage => Type usage -> Term usage -> (Term usage -> Term usage) -> Term usage
let' ty value f = makeLet ((name, one) ::: ty) value body where (name, body) = bindVariable f


abs' :: Constraint usage (Type usage) -> Term usage -> Term usage
abs' ((name, usage) ::: ty) body = Term (Abs ((name, usage) ::: cata Term ty) body)

lam :: Unital usage => Type usage -> (Term usage -> Term usage) -> Term usage
lam ty f = abs' ((name, one) ::: ty) body
  where (name, body) = bindVariable f

bindVariable :: (Term usage -> Term usage) -> (Name, Term usage)
bindVariable f = (name, body)
  where name = maybe (I 0) prime (maxBV body)
        body = f (var name)
        prime (I i) = I (succ i)
        prime (N s) = N (s <> "ʹ")

maxBV :: (Recursive t, Base t ~ Expr usage) => t -> Maybe Name
maxBV = cata $ \case
  (name, _) ::: ty :-> _ -> max (Just name) ty
  Abs ((name, _) ::: ty) _ -> max (Just name) ty
  other -> foldr max Nothing other


freeVariables :: (Recursive t, Base t ~ Expr usage) => t -> Set.Set Name
freeVariables = cata $ \case
  Var name -> Set.singleton name
  (name, _) ::: _ :-> body -> Set.delete name body
  Abs ((name, _) ::: _) body -> Set.delete name body
  other -> fold other


(#) :: Term usage -> Term usage -> Term usage
f # a = Term (App f a)

infixl 9 #


data Constraint usage recur = Binding usage ::: recur
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable Constraint where
  bifoldMap f g ((_, usage) ::: ty) = f usage <> g ty

instance Bifunctor Constraint where
  bimap f g ((name, usage) ::: ty) = (name, f usage) ::: g ty

instance Bitraversable Constraint where
  bitraverse f g ((name, usage) ::: ty) = (:::) . (,) name <$> f usage <*> g ty

infix 5 :::

constraintName :: Constraint usage recur -> Name
constraintName ((name, _) ::: _) = name

constraintValue :: Constraint usage recur -> recur
constraintValue (_ ::: ty) = ty

type Binding usage = (Name, usage)


instance Bifoldable Expr where
  bifoldMap f g = \case
    Unit         -> mempty
    UnitT        -> mempty
    BoolT        -> mempty
    Bool _       -> mempty
    TypeT        -> mempty
    Var _        -> mempty
    var :-> body -> bifoldMap f g var <> g body
    Abs var body -> bifoldMap f g var <> g body
    App f a      -> g f <> g a
    If c t e     -> g c <> g t <> g e
    a :* b       -> g a <> g b
    Pair a b     -> g a <> g b
    ExL a        -> g a
    ExR a        -> g a

instance Bifunctor Expr where
  bimap f g = \case
    Unit         -> Unit
    UnitT        -> UnitT
    BoolT        -> BoolT
    Bool b       -> Bool b
    TypeT        -> TypeT
    Var name     -> Var name
    var :-> body -> bimap f g var :-> g body
    Abs var body -> Abs (bimap f g var) (g body)
    App f a      -> App (g f) (g a)
    If c t e     -> If (g c) (g t) (g e)
    a :* b       -> g a :* g b
    Pair a b     -> Pair (g a) (g b)
    ExL a        -> ExL (g a)
    ExR a        -> ExR (g a)

instance Bitraversable Expr where
  bitraverse f g = \case
    Unit         -> pure Unit
    UnitT        -> pure UnitT
    BoolT        -> pure BoolT
    Bool b       -> pure (Bool b)
    TypeT        -> pure TypeT
    Var name     -> pure (Var name)
    var :-> body -> (:->) <$> bitraverse f g var <*> g body
    Abs var body -> Abs <$> bitraverse f g var <*> g body
    App f a      -> App <$> g f <*> g a
    If c t e     -> If <$> g c <*> g t <*> g e
    a :* b       -> (:*) <$> g a <*> g b
    Pair a b     -> Pair <$> g a <*> g b
    ExL a        -> ExL <$> g a
    ExR a        -> ExR <$> g a


instance Foldable Type where
  foldMap f = bifoldMap f (foldMap f) . unType

instance Functor Type where
  fmap f = Type . bimap f (fmap f) . unType

instance Traversable Type where
  traverse f = fmap Type . bitraverse f (traverse f) . unType


instance Foldable Term where
  foldMap f = bifoldMap f (foldMap f) . unTerm

instance Functor Term where
  fmap f = Term . bimap f (fmap f) . unTerm

instance Traversable Term where
  traverse f = fmap Term . bitraverse f (traverse f) . unTerm


newtype Silent usage = Silent { unSilent :: Expr usage (Silent usage) }

instance Show usage => Show (Silent usage) where
  showsPrec d = showsPrec d . unSilent
