module Reduction where

import Prog
import Term
import Substitutionen
import Matching
import Helper
import Positionen

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])     _ = Nothing
findRule (Prog (r:rs)) t = processMaybe 
                             (tryRule r t) (\x -> Just x) (findRule (Prog rs) t)
  where
    tryRule :: Rule -> Term -> Maybe (Rhs, Subst)
    tryRule (Rule l r') t' = processMaybe (match l t') 
                               (\x -> Just (r', x)) Nothing

reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt p t x = processMaybe (findRule p (selectAt t x)) 
                   (\(r, s) -> Just (apply s (replaceAt t x r))) Nothing

applyRule :: Term -> Rule -> Term
applyRule (Var v) (Rule l r) = if (l == (Var v)) then r else (Var v)
applyRule (Comb n xs) (Rule l r) = if (l == (Comb n xs)) 
                                   then r 
                                   else Comb n (map (\x -> applyRule x (Rule l r)) xs)

reduciblePos :: Prog -> Term -> [Pos]
reduciblePos p t = filter (\x -> not(isNothing(reduceAt p t x))) (allPos t)

isNormalForm :: Prog -> Term -> Bool
isNormalForm p t = isNothing(findRule p t)