module Reduction(findRule, reduceAt, reduciblePos, isNormalForm) where

import Prog
import Term
import Substitutionen
import Positionen

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule _ _ = Nothing

reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt _ _ _ = Nothing

reduciblePos :: Prog -> Term -> [Pos]
reduciblePos _ _ = []

isNormalForm :: Prog -> Term -> Bool
isNormalForm _ _ = False