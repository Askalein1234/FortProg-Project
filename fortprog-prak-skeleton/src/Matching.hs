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

match1 :: Term -> Term -> Maybe Subst
match1 (Var v)     t           = Just $ single v t
match1 (Comb _ _)  (Var _)     = Nothing
match1 (Comb _ xs) (Comb _ ys) = matchList xs ys
    where
        matchList []     _      = Nothing
        matchList _      []     = Nothing
        matchList (x:xs) (y:ys) = let rest = matchList xs ys
                                      head = match x y in
                                    case rest of 
                                        Nothing -> head
                                        Just t  -> case head of
                                                    Nothing -> rest
                                                    Just t' -> Just $ t' . t
