module Positionen where
import Term
import Substitutionen

type Pos = [Int]

above :: Pos -> Pos -> Bool
above xs ys = (length xs) < (length ys)

below :: Pos -> Pos -> Bool
below xs ys = (length xs) > (length ys)

leftOf :: Pos -> Pos -> Bool
leftOf xs [] = False
leftOf [] ys = False
leftOf (x:xs) (y:ys) = if x == y 
                         then leftOf xs ys
                         else x < y

rightOf :: Pos -> Pos -> Bool
rightOf xs [] = False
rightOf [] ys = False
rightOf (x:xs) (y:ys) = if x == y 
                          then rightOf xs ys
                          else x > y

selectAt :: Term -> Pos -> Term
selectAt (Var v)     _      = Var v
selectAt (Comb n xs) []     = Comb n xs
selectAt (Comb n xs) (p:ps) = selectAt (xs!!p) ps

replaceAt :: Term -> Pos -> Term -> Term
replaceAt _ []               t = t
replaceAt (Var v)     ps     t = Var v
replaceAt (Comb n xs) (p:ps) t = Comb n (map (\x -> if (x == xs!!p)
                                                    then replaceAt x ps t
                                                    else x) 
                                              xs)

allPos :: Term -> [Pos]
allPos t = map (\x -> take (length x - 1) x) (allPosWithPlaceholder t)
 where
    allPosWithPlaceholder (Var v)     = []
    allPosWithPlaceholder (Comb n xs) = concatMap 
                                        (\x -> map (\y -> x : y) ([-1] : allPosWithPlaceholder (xs!!x))) 
                                        [0..((length xs) - 1)]
