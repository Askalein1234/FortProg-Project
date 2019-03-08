module Positionen(
  above,
  below,
  leftOf,
  rightOf,
  selectAt,
  replaceAt,
  allPos,
  Pos) where

import Term

import Helper

-- representation for Positions in Terms, each Number for
-- the index at that lvl in Term
type Pos = [Int]

-- if pos2 is a subterm of pos1
above :: Pos -> Pos -> Bool
-- if pos2 is root of subterm, pos1 can't be above
above _      []     = False
-- if pos1 is root of subterm and pos2 is not, pos1 is above pos2
above []     (_:_)  = True
-- if none is root and both are part of same subterm, check subterm
above (x:xs) (y:ys) 
  | x == y          = above xs ys
  | otherwise       = False

-- if pos2 is above pos1, pos1 is below pos2
below :: Pos -> Pos -> Bool
below xs ys = above ys xs

leftOf :: Pos -> Pos -> Bool
-- if any is root, they are in the same subterm
-- and therefore not right or left of each other
leftOf _      []     = False
leftOf []     _      = False
leftOf (x:xs) (y:ys)
  -- if they are the same subterm, we have to check the subterm
  | x == y           = leftOf xs ys
  -- otherwise the smaller one is left
  | otherwise        = x < y

-- if pos2 is left of pos1, pos1 is right of pos2
rightOf :: Pos -> Pos -> Bool
rightOf xs ys = leftOf ys xs

-- get subterm at Pos (or deepest subterm on the way there)
selectAt :: Term -> Pos -> Term
-- can't got deeper at Var
selectAt (Var v)     _      = Var v
-- found it
selectAt (Comb n xs) []     = Comb n xs
-- look deeper
selectAt (Comb _ xs) (p:ps) = selectAt (xs!!p) ps

-- replace subterm at Pos if Pos exists
replaceAt :: Term -> Pos -> Term -> Term
-- found it, replace it
replaceAt _           []     t = t
-- can't go deeper, nothing to replace
replaceAt (Var v)     _      _ = Var v
-- go deeper
replaceAt (Comb n xs) (p:ps) t = 
  Comb n (replaceElem p (\x -> replaceAt x ps t) xs)

-- get all available Positions in Term
allPos :: Term -> [Pos]
allPos t = [] : (map (\x -> take (length x - 1) x) (allPosWithPlaceholder t))
  where
    allPosWithPlaceholder (Var _)     = []
    allPosWithPlaceholder (Comb _ xs) 
      = concatMap (\x -> map (\y -> x : y) 
                  ([-1] : allPosWithPlaceholder (xs!!x))) 
                  [0..((length xs) - 1)]
