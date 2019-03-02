module Reduction where

import Prog
import Term
import Substitutionen
import Matching
import Helper

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])   t = Nothing
findRule (Prog (r:rs)) t = let try = tryRule r t in
                             case try of
                               Nothing -> findRule (Prog rs) t
                               Just _  -> try
  where
    tryRule :: Rule -> Term -> Maybe (Rhs, Subst)
    tryRule (Rule l r) t = let res = match t l in 
                             case res of
                               Nothing -> Nothing
                               Just _  -> Just (r, fromJust res)
                               
