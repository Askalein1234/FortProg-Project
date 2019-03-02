module Reduction where

import Prog
import Term
import Substitutionen
import Matching
import Helper
import Positionen

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])     t = Nothing
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
                               
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt p t x = let res = findRule p (selectAt t x) in
                   case res of 
                     Nothing -> Nothing
                     Just _  -> Just (selectAt t x)
                     
reduciblePos :: Prog -> Term -> [Pos]
reduciblePos p t = filter (\x -> not(isNothing(reduceAt p t x))) (allPos t)

-- Definition 3.9 (Normalform) Ein Term t heißt in Normalform, falls es keinen Term t' gibt mit t ⇒ t', d. h. falls kein weiterer Reduktionsschritt möglich ist
isNormalForm :: Prog -> Term -> Bool
isNormalForm p t = isNothing(findRule p t)