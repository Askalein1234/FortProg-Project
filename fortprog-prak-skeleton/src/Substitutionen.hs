module Substitutionen where
import Term

data Subst = Subst (Term -> Term)

identity :: Subst
identity = Subst (\x -> x)

single :: VarName -> Term -> Subst

apply :: Subst -> Term -> Term
apply (Subst s) = s

compose :: Subst -> Subst -> Subst
compose (Subst f) (Subst g) = Subst (f . g)