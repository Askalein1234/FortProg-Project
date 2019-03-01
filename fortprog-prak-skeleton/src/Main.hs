module Main where

import Matching(match)
import Parser()
import Positionen()
import PrettyPrinting()
import Prog()
import Substitutionen(Subst, apply)
import Term(Term(..))

import Helper(fromJust)

main :: IO ()
main = putStrLn (show test)

term1 :: Term
term1 = Comb "Test1" [Comb "Test2" [Var "Test4", Var "Test5"], Comb "Test3" [Var "Test6"]]
term2 :: Term
term2 = Comb "Test1" [Comb "Test2" [Comb "Test4" [Var "Test7"], Comb "Test5" [Var "Test8"]], Comb "Test3" [Comb "Test6" [Var "Test9"]]]

test :: Term
test = apply (fromJust subst) term1

subst :: Maybe Subst
subst = match term1 term2