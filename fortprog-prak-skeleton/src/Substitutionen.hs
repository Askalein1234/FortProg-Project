module Substitutionen where
import Term

type Subst = VarName -> Term

identity :: Subst
identity = \x -> Var x

single :: VarName -> Term -> Subst
single y t = \x -> if x == y then t else Var y

apply :: Subst -> Term -> Term
apply s (Var v)     = s v
apply s (Comb x xs) = Comb x $ map (apply s) xs

compose :: Subst -> Subst -> Subst
compose f g = \x -> apply g $ apply f (Var x)
