module Manifold.Proof.Focusing where

data Linear prop
  = LEmpty
  | prop :<| Linear prop
