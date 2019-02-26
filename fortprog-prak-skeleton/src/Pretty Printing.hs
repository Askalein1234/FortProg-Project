module Pretty_Printing where

import Term
import Data.List

class Pretty a where
    pretty :: a -> String
    
instance Pretty Term where
    pretty (Var name) = name
    pretty (Comb name []) = name
    pretty (Comb name ts) = name ++ " " ++
                                intercalate " " (map (\t ->
                                    case t of
                                        Comb name [] -> name
                                        Comb name (t':_) -> "(" ++ name ++ " " ++ pretty t' ++ ")"
                                        Var name -> name) ts)