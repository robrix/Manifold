{-# LANGUAGE GADTs #-}
module Manifold.REPL where

import Manifold.Expr

data REPL usage result where
  TypeOf :: Term usage -> REPL usage (Type usage)
  Quit :: REPL usage ()
