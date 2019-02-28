module Matching where
import Term
import Substitutionen

match :: Term -> Term -> Maybe Subst
match (Var v) t               = Just (single v t)
match (Comb n xs) (Var v)     = Nothing
match (Comb n xs) (Comb m ys) = if (length xs) == (length ys) && 
                                    notContainsNothing (map (\x -> match (xs!!x) (ys!!x)) [0..((length xs) - 1)])
                                then Just (foldr (.) id (map (\x -> fromJust(match (xs!!x) (ys!!x))) [0..((length xs) - 1)]))
                                else Nothing
  where
    notContainsNothing :: [Maybe Subst] -> Bool
    notContainsNothing [] = True
    notContainsNothing (x:xs) = case x of 
                                  Just a  -> notContainsNothing xs
                                  Nothing -> False

fromJust :: Maybe a -> a
fromJust (Just b) = b