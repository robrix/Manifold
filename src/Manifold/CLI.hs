{-# LANGUAGE GADTs #-}
module Manifold.CLI where

data Command result where
  Interactive :: Command ()
