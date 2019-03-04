module Evaluation where

import Prog
import Term
import Substitutionen
import Matching
import Helper
import Positionen
import Reduction
import Data.List

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

loStrategy :: Strategy
loStrategy = \p t -> [head (sortBy comparator (reduciblePos p t))]
  where
    comparator []     []     = compare 1 0
    comparator (x:xs) []     = compare 1 0 -- Hier wird der längere gewählt, ist aber glaube ich egal
    comparator []     (y:ys) = compare 0 1
    comparator (x:xs) (y:ys) = if x == y then comparator xs ys else compare x y
    
liStrategy :: Strategy
liStrategy = \p t -> [head (sortBy comparator (reduciblePos p t))]
  where
    comparator []     []     = compare 1 0
    comparator (x:xs) []     = compare 1 0 -- Hier wird der längere gewählt, ist aber glaube ich egal
    comparator []     (y:ys) = compare 0 1
    comparator (x:xs) (y:ys) = if x == y then comparator xs ys else compare x y