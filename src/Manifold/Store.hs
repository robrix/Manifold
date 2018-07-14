module Manifold.Store where

import qualified Data.Map.Monoidal as Monoidal

newtype Store address cell = Store { unStore :: Monoidal.Map address cell }
