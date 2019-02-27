module Substitutionen where
import Term

data Subst = Subst (Term -> Term)

identity :: Subst
identity = Subst (\x -> x)

single :: VarName -> Term -> Subst
single y t = Subst $ replace y t
  where
      replace y t x = case x of 
                        Var v -> if (v == y) 
                                   then t
                                   else Var v
                        Comb c ts -> Comb c (map (replace y t) ts)

identity :: Subst
identity = Subst (\x -> x)

apply :: Subst -> Term -> Term
apply (Subst s) = s

compose :: Subst -> Subst -> Subst
compose (Subst f) (Subst g) = Subst (f . g)
