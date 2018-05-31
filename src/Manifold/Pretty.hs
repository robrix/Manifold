{-# LANGUAGE DefaultSignatures #-}
module Manifold.Pretty where

import Text.Show

class Pretty a where
  prettyPrec :: Int -> a -> ShowS
  default prettyPrec :: Show a => Int -> a -> ShowS
  prettyPrec = showsPrec

prettys :: Pretty a => a -> ShowS
prettys = prettyPrec 0

pretty :: Pretty a => a -> String
pretty = ($ "") . prettys

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = putStrLn . pretty


showSpace :: ShowS -> ShowS
showSpace s = showChar ' ' . s . showChar ' '

showBrace :: Bool -> ShowS -> ShowS
showBrace True  s = showChar '{' . s . showChar '}'
showBrace False s = s


instance Pretty ()

instance Pretty a => Pretty [a] where
  prettyPrec _ = showListWith (prettyPrec 0)
