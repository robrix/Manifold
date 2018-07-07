{-# LANGUAGE DeriveFunctor, LambdaCase, TypeFamilies #-}
module Manifold.Term where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable (fold)
import Data.Functor.Foldable (Base, Corecursive(..), Recursive(..))
import qualified Data.Set as Set
import Data.Trifoldable
import Data.Trifunctor
import Manifold.Name
import Manifold.Pattern
import Manifold.Pretty
import Manifold.Term.Elim
import Manifold.Term.Intro

newtype Term var = Term { unTerm :: Expr var (Term var) }
  deriving (Eq, Ord, Show)

type instance Base (Term var) = Expr var

instance Recursive   (Term var) where project = unTerm
instance Corecursive (Term var) where embed   =   Term

instance Pretty var => Pretty (Term var) where
  prettyPrec d = prettyPrec d . unTerm


newtype Elab var ann = Elab { unElab :: ElabExpr var ann (Elab var ann) }
  deriving (Eq, Ord, Show)

type instance Base (Elab var ann) = ElabExpr var ann

instance Recursive   (Elab var ann) where project = unElab
instance Corecursive (Elab var ann) where embed   =   Elab

instance (Pretty var, Pretty ann) => Pretty (Elab var ann) where
  prettyPrec d = prettyPrec d . unElab


data ElabExpr var ann recur = ElabExpr { elabExpr :: Expr var recur, elabAnn :: ann }
  deriving (Eq, Functor, Ord, Show)

instance (Pretty var, Pretty ann, Pretty recur) => Pretty (ElabExpr var ann recur) where
  prettyPrec d (ElabExpr expr ann) = prettyParen (d > 0) $ prettyPrec 0 expr <+> prettyString ":" <+> prettyPrec 0 ann


delaborate :: Elab var ann -> Term var
delaborate = cata (Term . elabExpr)


var :: Name -> Term var
var = Term . Var

intro :: Intro var (Term var) (Term var) -> Term var
intro = Term . Value

elim :: Elim (Term var) -> Term var
elim = Term . Elim


unit :: Term var
unit = data' (N "Unit") []

true :: Term var
true = data' (N "True") []

false :: Term var
false = data' (N "False") []

iff :: Term var -> Term var -> Term var -> Term var
iff c t e = elim (If c t e)


pair :: Term var -> Term var -> Term var
pair a b = data' (N "Pair") [a, b]

exl :: Term var -> Term var
exl = (var (N "exl") #)

exr :: Term var -> Term var
exr = (var (N "exr") #)


data' :: Name -> [Term var] -> Term var
data' c ts = intro (Data c ts)


makeLet :: var -> Term var -> Term var -> Term var
makeLet var value body = makeAbs var body # value

let' :: Term Name -> (Term Name -> Term Name) -> Term Name
let' value f = makeLet name value body where (name, body) = bindVariable f


makeAbs :: var -> Term var -> Term var
makeAbs var body = intro (Abs var body)

lam :: (Term Name -> Term Name) -> Term Name
lam f = makeAbs name body where (name, body) = bindVariable f


(#) :: Term var -> Term var -> Term var
f # a = elim (App f a)

infixl 9 #


case' :: Term var -> [(Pattern Name, Term var)] -> Term var
case' s bs = elim (Case s bs)


data Expr var recur
  = Var Name
  | Value (Intro var recur recur)
  | Elim (Elim recur)
  deriving (Eq, Ord, Show)

instance Bifoldable Expr where
  bifoldMap f g = \case
    Var _   -> mempty
    Value i -> trifoldMap f g g i
    Elim e  -> foldMap g e

instance Foldable (Expr var) where
  foldMap = bifoldMap (const mempty)

instance Bifunctor Expr where
  bimap f g = \case
    Var n   -> Var n
    Value i -> Value (trimap f g g i)
    Elim e  -> Elim (fmap g e)

instance Functor (Expr var) where
  fmap = bimap id

instance (Pretty var, Pretty recur) => Pretty (Expr var recur) where
  prettyPrec d (Var n)    = prettyPrec d n
  prettyPrec d (Value  i) = prettyPrec d i
  prettyPrec d (Elim e)   = prettyPrec d e


bindVariable :: Named var => (Term var -> Term var) -> (Name, Term var)
bindVariable f = (n, body)
  where n = maybe (I 0) prime (maxBV body)
        body = f (embed (Var n))
        prime (I i) = I (succ i)
        prime (N s) = N (s <> "ʹ")
        prime (Q s n) = Q s (prime n)

maxBV :: Named var => Term var -> Maybe Name
maxBV = cata $ \case
  Value (Abs var _) -> Just (name var)
  other -> foldr max Nothing other

freeVariables :: Named var => Term var -> Set.Set Name
freeVariables = cata $ \case
  Var name -> Set.singleton name
  Value (Abs var body) -> Set.delete (name var) body
  other -> fold other
