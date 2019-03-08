module Matching(match) where

import Term

import Helper
import Substitutionen

-- let's try to make Term 2 from Term 1 by replacing Vars with other Terms
match :: Term -> Term -> Maybe Subst
-- it looks like we just found a replaceable Var
match (Var v)     t           = Just $ single v t
-- it looks like there is no replacing for there's no term at any Var below
-- Term 1
match (Comb _ _)  (Var _)     = Nothing
-- it looks like we have to keep looking deeper
match (Comb n xs) (Comb m ys) 
  -- if they are identical 'til here, we have to search their subtrees
  | m == n                    = matchList xs ys
  -- if not, there is no match
  | otherwise                 = Nothing
    where
      matchList xs' ys' =
        -- if the length is the same, there could be a match
        if length xs' == length ys' && 
           -- if there is some Nothing resulting, we can't match the whole Terms
           -- but if there is just Justs by matching the subterms
           notContainsNothing
           (map (\x -> match (xs'!!x) (ys'!!x))
           [0..(length xs' - 1)])
          -- compose the subterm matches
          then Just $ foldr compose identity 
            (map (\x -> fromJust (match (xs'!!x) (ys'!!x)))
            [0..(length xs' - 1)])
          -- otherwise there is no match
          else Nothing

      -- if there is only Just in a list of Maybe
      notContainsNothing :: [Maybe a] -> Bool
      notContainsNothing []      = True
      notContainsNothing (x:xs') = case x of 
                                     Just _  -> notContainsNothing xs'
                                     Nothing -> False
