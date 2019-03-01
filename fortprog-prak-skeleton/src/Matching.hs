module Matching where
import Term
import Substitutionen

match :: Term -> Term -> Maybe Subst
match (Var v)     t           = Just $ single v t
match (Comb _ _)  (Var _)     = Nothing
match (Comb m xs) (Comb m ys) = if m == n then matchList xs ys else Nothing
    where
        matchList []     _      = Nothing
        matchList _      []     = Nothing
        matchList (x:xs) (y:ys) = case (matchList xs ys, match x y) of 
                                    (Just t, Just t') -> Just $ t' . t
                                    (_, _)            -> Nothing
