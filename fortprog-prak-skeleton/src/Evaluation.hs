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
poStrategy p t = 
  let res = reduciblePos p t 
  in if elem [] res
      then [[]]
      else filter (\x -> foldl (\x' y -> x' && (not $ above x y)) True res) res

piStrategy :: Strategy
piStrategy p t = 
  let res = reduciblePos p t 
  in  filter (\x -> foldl (\x' y -> x' && (not $ below x y)) True res) res

-- True/False
--       L/R     O/I     Output
strat :: Bool -> Bool -> Strategy
strat lr oi = \p t -> [head (sortBy (comparator lr oi) (reduciblePos p t))]
  where
    comparator :: Bool -> Bool -> Pos -> Pos -> Ordering
    comparator lr' oi' p1 p2
      | lr' &&      (leftOf  p1 p2) = LT
      | lr' &&      (rightOf p1 p2) = GT
      | not(lr') && (leftOf  p1 p2) = GT
      | not(lr') && (rightOf p1 p2) = LT
      | oi' &&      (above   p1 p2) = LT
      | oi' &&      (below   p1 p2) = GT
      | not(oi') && (above   p1 p2) = GT
      | not(oi') && (below   p1 p2) = LT
      | otherwise                   = comparator lr' oi' (tail p1) (tail p2)

reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith s p t = if (null(s p t)) 
                   then Nothing 
                   else reduceAll (s p t) p t
  where
    reduceAll :: [Pos] -> Prog -> Term -> Maybe Term
    reduceAll []     _  t' = Just t'
    reduceAll (r:rs) p' t' = case (reduceAt p' t' r) of
                               Nothing -> reduceAll rs p' t'
                               Just a  -> reduceAll rs p' a
                             
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith s p t = if (isNormalForm p t)
                     then t
                     else case (reduceWith s p t) of
                            Nothing -> t
                            Just a  -> evaluateWith s p a