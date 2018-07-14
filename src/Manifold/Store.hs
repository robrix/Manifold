module Manifold.Store where

import qualified Data.Map.Monoidal as Monoidal

newtype Store addr cell = Store { unStore :: Monoidal.Map addr cell }
