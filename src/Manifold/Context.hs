module Manifold.Context where

import Manifold.Name
import Manifold.Type

data Context
  = Empty

data Constraint = Name ::: Type
