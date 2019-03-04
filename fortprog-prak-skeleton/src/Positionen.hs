module Positionen(above, below, leftOf, rightOf, selectAt, replaceAt, allPos, Pos) where
import Term
import Helper

type Pos = [Int]

above :: Pos -> Pos -> Bool
above []     []     = False
above []     (_:_)  = True
above (_:_)  []     = False
above (x:xs) (y:ys) 
    | x == y        = above xs ys
    | otherwise     = False

below :: Pos -> Pos -> Bool
below xs ys = above ys xs

leftOf :: Pos -> Pos -> Bool
leftOf _ [] = False
leftOf [] _ = False
leftOf (x:xs) (y:ys) = if x == y 
                         then leftOf xs ys
                         else x < y

rightOf :: Pos -> Pos -> Bool
rightOf _ [] = False
rightOf [] _ = False
rightOf (x:xs) (y:ys) = if x == y 
                          then rightOf xs ys
                          else x > y

selectAt :: Term -> Pos -> Term
selectAt (Var v)     _      = Var v
selectAt (Comb n xs) []     = Comb n xs
selectAt (Comb _ xs) (p:ps) = selectAt (xs!!p) ps

replaceAt :: Term -> Pos -> Term -> Term
replaceAt _           []     t = t
replaceAt (Var v)     _      _ = Var v
replaceAt (Comb n xs) (p:ps) t = Comb n (replaceElem p (\x -> replaceAt x ps t) xs)

allPos :: Term -> [Pos]
allPos t = [] : (map (\x -> take (length x - 1) x) (allPosWithPlaceholder t))
  where
    allPosWithPlaceholder (Var _)     = []
    allPosWithPlaceholder (Comb _ xs) = concatMap 
                                        (\x -> map (\y -> x : y) 
                                          ([-1] : allPosWithPlaceholder (xs!!x))) 
                                        [0..((length xs) - 1)]
