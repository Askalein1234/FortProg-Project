module Matching(match) where

import Term

import Helper
import Substitutionen

match :: Term -> Term -> Maybe Subst
match (Var v)     t           = Just $ single v t
match (Comb _ _)  (Var _)     = Nothing
match (Comb n xs) (Comb m ys) 
  | m == n                    = matchList xs ys
  | otherwise                 = Nothing
    where
      matchList xs' ys' =
        if length xs' == length ys' && 
           notContainsNothing
           (map (\x -> match (xs'!!x) (ys'!!x))
           [0..(length xs' - 1)])
          then Just $ foldr compose identity 
            (map (\x -> fromJust (match (xs'!!x) (ys'!!x)))
            [0..(length xs' - 1)])
          else Nothing

      notContainsNothing :: [Maybe Subst] -> Bool
      notContainsNothing []      = True
      notContainsNothing (x:xs') = case x of 
                                     Just _  -> notContainsNothing xs'
                                     Nothing -> False
