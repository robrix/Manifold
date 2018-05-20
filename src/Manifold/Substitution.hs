module Manifold.Substitution where

import Manifold.Name

newtype Substitution term = Substitution { getSubstitution :: [(Name, term)] }
