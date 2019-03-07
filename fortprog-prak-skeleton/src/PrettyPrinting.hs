module PrettyPrinting(Pretty(..)) where

import Data.List

import Term

class Pretty a where
  pretty :: a -> String
    
-- make a Term Pretty
instance Pretty Term where
  pretty (Var  name)    = name
  pretty (Comb name []) = name
  pretty (Comb name ts)
    = name ++ " " ++ intercalate " " (
      map (\t -> case t of
                   Comb name' []    -> name'
                   Comb _     (_:_) -> "(" ++ pretty t ++ ")"
                   Var  name'       -> name') ts)