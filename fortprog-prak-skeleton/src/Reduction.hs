module Reduction where

import Prog
import Term

import Helper
import Matching
import Positionen
import Substitutionen

-- find first appliable rule to Term if one exists
findRule :: Prog -> Term -> Maybe (Rhs, Subst)
findRule (Prog [])     _
  = Nothing
findRule (Prog (r:rs)) t
  -- if this rule is it, return right side and Subst, else try the other rules
  = processMaybe (tryRule r t) (\x -> Just x) (findRule (Prog rs) t)
  where
    -- try a rule
    tryRule :: Rule -> Term -> Maybe (Rhs, Subst)
    tryRule (Rule l r') t' 
      = processMaybe (match l t') (\x -> Just (r', x)) Nothing

-- reduce Term at Pos if possible
reduceAt :: Prog -> Term -> Pos -> Maybe Term
reduceAt p t x
  -- find a rule, replace with right side and apply Subst if such a rule exists
  = processMaybe (findRule p (selectAt t x)) 
  (\(r, s) -> Just (apply s (replaceAt t x r))) Nothing

-- apply a specific rule to a Term (if no matching rule exists for any Subterm,
-- no changes will be made)
applyRule :: Term -> Rule -> Term
applyRule t@(Var _)     (Rule l r) = if (l == t) then r else t
applyRule t@(Comb n xs) (Rule l r) = if (l == t) 
                                       then r 
                                       else 
                                         Comb n 
                                         (map 
                                         (\x -> applyRule x (Rule l r)) xs)

-- return all Pos where Term is reducible (kinda obvious with this name)
reduciblePos :: Prog -> Term -> [Pos]
-- just simply by filtering if any rule is appliable at Pos
reduciblePos p t = filter (not . isNothing . reduceAt p t) $ allPos t

-- if Term is not reducible
isNormalForm :: Prog -> Term -> Bool
-- just by looking at reduciblePos emptyness
isNormalForm p t = null(reduciblePos p t)