module PrettyPrinting(Pretty(..)) where

import Term
import Data.List

class Pretty a where
    pretty :: a -> String
    
-- make a Term Pretty
instance Pretty Term where
    pretty (Var name) = name
    pretty (Comb name []) = name
    pretty (Comb name ts) = name ++ " " ++
                                intercalate " " (map (\t ->
                                    case t of
                                        Comb name' []     -> name'
                                        Comb name' (t':_) -> "(" ++ name' ++ " " ++ pretty t' ++ ")"
                                        Var  name'        -> name') ts)