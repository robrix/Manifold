module Manifold.Declaration where

import Manifold.Term
import Manifold.Type

data Declaration var
  = Binding Term (Type var)
