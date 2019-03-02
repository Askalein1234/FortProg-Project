module Reduction where

import Prog
import Matching

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])   t = Nothing
findRule (Prog r:rs) t = if (tryRule r t) == Nothing 
                           then findRule (Prog rs) t 
                           else Just (tryRule r t)
  where
    tryRule :: Rule -> Term -> Maybe (Rhs, Subst)
    tryRule (Rule l r) t = let res = match t l in 
                             if res == Nothing
                               then Nothing
                               else Just (r, res)
                               
