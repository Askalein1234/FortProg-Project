module PrettyPrinting(Pretty(..)) where

import Data.List

import Term
import Prog

class Pretty a where
  pretty :: a -> String
    
-- make a Term Pretty
instance Pretty Term where
  -- it's a Var? Just the name.
  pretty (Var  name)
    = name
  -- Comb without Children? Literal, so just the name
  pretty (Comb name [])
    = name
  -- Comb with children? Name and space separated arguments
  pretty (Comb name ts)
    = name ++ " " ++ intercalate " " (
      map (\t -> case t of
                   -- if it's a Comb without children, name
                   Comb name' []    -> name'
                   -- if it's a Comb with children, pretty it with brackets
                   Comb _     (_:_) -> "(" ++ pretty t ++ ")"
                   -- if it's a Var, you know it, just the name
                   Var  name'       -> name') ts)

instance Pretty Rule where
  -- Just pretty the two sides and put an equal sign between
  pretty (Rule t1 t2) = pretty t1 ++ " = " ++ pretty t2

instance Pretty Prog where
  -- just the pretty rules with linebreaks
  pretty (Prog xs) = intercalate "\n" $ map pretty xs
