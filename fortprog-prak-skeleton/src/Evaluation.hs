module Evaluation where

import Prog
import Term
import Positionen
import Reduction
import Data.List

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy = strat True True

liStrategy :: Strategy
liStrategy = strat True False

roStrategy :: Strategy
roStrategy = strat False True

riStrategy :: Strategy
riStrategy = strat False False

poStrategy :: Strategy
poStrategy = \p t -> let res = reduciblePos p t in 
                       filter (\x -> (length x) == minimum(map length res)) res

piStrategy :: Strategy
piStrategy = \p t -> let res = reduciblePos p t in 
                       filter (\x -> (length x) == maximum(map length res)) res

-- True/False
--       L/R     O/I     Output
strat :: Bool -> Bool -> Strategy
strat lr oi = \p t -> [head (sortBy (comparator lr oi) (reduciblePos p t))]
  where
    comparator :: Bool -> Bool -> Pos -> Pos -> Ordering
    comparator _   _   []     []     = compare (1::Int) (0::Int)
    comparator _   oi' (_:_)  []     = if (oi') then compare (1::Int) (0::Int) else compare (0::Int) (1::Int)
    comparator _   oi' []     (_:_)  = if (oi') then compare (0::Int) (1::Int) else compare (1::Int) (0::Int)
    comparator lr' oi'  (x:xs) (y:ys) = if (length xs) == (length ys) 
                                      then (if x == y 
                                            then (comparator lr' oi' xs ys) 
                                            else (if (lr')
                                                  then compare x y
                                                  else compare y x)) 
                                      else (if (oi') 
                                            then compare (length xs) (length ys) 
                                            else compare (length ys) (length xs))

reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith s p t = if (null(s p t)) 
                   then Nothing 
                   else reduceAll (s p t) p t
  where
    reduceAll :: [Pos] -> Prog -> Term -> Maybe Term
    reduceAll []     _  t' = Just t'
    reduceAll (r:rs) p' t' = case (reduceAt p' t' r) of
                               Nothing -> Just t'
                               Just a  -> reduceAll rs p' a
                             
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith s p t = if (isNormalForm p t)
                     then t
                     else case (reduceWith s p t) of
                            Nothing -> t
                            Just a  -> evaluateWith s p a