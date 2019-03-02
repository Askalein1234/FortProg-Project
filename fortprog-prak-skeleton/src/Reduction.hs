module Reduction where

import Prog
import Term
import Substitutionen
import Matching
import Helper
import Positionen

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])     _ = Nothing
findRule (Prog (r:rs)) t = processMaybe (tryRule r t) (\x -> Just x) (findRule (Prog rs) t)
  where
    tryRule :: Rule -> Term -> Maybe (Rhs, Subst)
    tryRule (Rule l r') t' = processMaybe (match t' l) (\x -> Just (r', x)) Nothing
                               
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt p t x = processMaybe (findRule p (selectAt t x)) (\_ -> Just (selectAt t x)) Nothing
                     
reduciblePos :: Prog -> Term -> [Pos]
reduciblePos p t = filter (\x -> not(isNothing(reduceAt p t x))) (allPos t)

-- Definition 3.9 (Normalform) Ein Term t heißt in Normalform, falls es keinen Term t' gibt mit t ⇒ t', d. h. falls kein weiterer Reduktionsschritt möglich ist
isNormalForm :: Prog -> Term -> Bool
isNormalForm p t = isNothing(findRule p t)