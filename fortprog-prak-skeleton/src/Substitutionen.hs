module Substitutionen where
import Term

type Subst = Term -> Term

identity :: Subst
identity = \x -> x

single :: VarName -> Term -> Subst
single y t = replace y t
  where
      replace y t x = case x of 
                        Var v -> if (v == y) 
                                   then t
                                   else Var v
                        Comb c ts -> Comb c (map (replace y t) ts)

apply :: Subst -> Term -> Term
apply s = s

compose :: Subst -> Subst -> Subst
compose f g = f . g
