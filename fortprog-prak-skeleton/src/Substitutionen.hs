module Substitutionen(
  identity,
  single,
  apply,
  compose,
  Subst) where

import Term

type Subst = VarName -> Term

-- Subst that replaces a Var with itself
identity :: Subst
identity = \x -> Var x

-- Subst that replaces a Var with a given Term
single :: VarName -> Term -> Subst
single y t = \x -> if x == y then t else Var x

-- apply a Subst to a Term
apply :: Subst -> Term -> Term
-- if it's a Var, just apply the subst to the given name
apply s (Var v)     = s v
-- if it's a Comb, map it to the subterm to recursivly apply it to all Vars
apply s (Comb x xs) = Comb x $ map (apply s) xs

-- apply first Subst first, then the other
compose :: Subst -> Subst -> Subst
compose f g = \x -> apply f $ apply g (Var x)
