module Main where

import Matching
import Parser
import Positionen
import PrettyPrinting
import Prog
import Substitutionen
import Term

import Helper

main = putStrLn (show test)

term1 :: Term
term1 = Comb "Test1" [Comb "Test2" [Var "Test4", Var "Test5"], Comb "Test3" [Var "Test6"]]
term2 :: Term
term2 = Comb "Test1" [Comb "Test2" [Comb "Test4" [Var "Test7"], Comb "Test5" [Var "Test8"]], Comb "Test3" [Comb "Test6" [Var "Test9"]]]

test = apply (fromJust subst) term1

subst = match term1 term2

compTest = compose (single "Test4" (Comb "kek1" [])) (single "Test6" (Var "TEASTES"))
singleApplyTest = apply (single "kek" $ Comb "kek2" [Var "kek3"]) (Comb "comberino" [Var "kek", Var "kek3"])
applyTest = apply (\x -> if x == "kek" then Comb "kek2" [Var "kek3"] else Var x) (Comb "comberino" [Var "kek", Var "kek3"])