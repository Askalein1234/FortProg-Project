module Evaluation where

import Data.List

import Prog
import Term

import Positionen
import Reduction

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

-- the leftmost outermost strategie
loStrategy :: Strategy
loStrategy = strat True True

-- the leftmost innermost strategie
liStrategy :: Strategy
liStrategy = strat True False

-- the rightmost outermost strategie
roStrategy :: Strategy
roStrategy = strat False True

-- the rightmost innermost strategie
riStrategy :: Strategy
riStrategy = strat False False

-- the leftmost outermost strategie
poStrategy :: Strategy
poStrategy p t = 
  let res = reduciblePos p t 
  -- for some reason po doesn't work if root os reducible,
  -- so catch that case first. (pi work's anyways)
  in if elem [] res
       then 
         [[]]
       else
         -- filter the ones with none above
         -- if some is above, the fold is False
         filter (\x -> foldl (\x' y -> x' && (not $ above x y)) True res) res

-- the leftmost outermost strategie
piStrategy :: Strategy
piStrategy p t = 
  let res = reduciblePos p t 
  -- filter the ones with none below
  -- if some is below, the fold is False
  in  filter (\x -> foldl (\x' y -> x' && (not $ below x y)) True res) res

-- sort the reduciblePos by l/r strategy ans take the head for evaluation
-- True/False
--       L/R     O/I     Output
strat :: Bool -> Bool -> Strategy
strat lr oi = \p t -> [head (sortBy (comparator lr oi) (reduciblePos p t))]
  where
    -- define ordering depending on strategy
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

-- if possible, reduce with strategy once
reduceWith :: Strategy -> Prog -> Term -> Maybe Term
reduceWith s p t = if (null(s p t)) 
                     then Nothing 
                     else reduceAll (s p t) p t
 where
  -- take a list of Pos to reduce at (usually calculated by strategy)
  -- and reduce at all of them, Nothing if not reducible (whyever that could be)
  reduceAll :: [Pos] -> Prog -> Term -> Maybe Term
  reduceAll []     _  t' = Just t'
  reduceAll (r:rs) p' t' = case (reduceAt p' t' r) of
                             -- if here not possible, do next one
                             Nothing -> reduceAll rs p' t'
                             -- if here possible, do next with result of it
                             Just a  -> reduceAll rs p' a

-- reduce until normal form
evaluateWith :: Strategy -> Prog -> Term -> Term
evaluateWith s p t = if (isNormalForm p t)
                       then t
                       else case (reduceWith s p t) of
                              Nothing -> t
                              Just a  -> evaluateWith s p a