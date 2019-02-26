module Substitutionen where
import Term

data Subst = Subst (Term -> Term)

identity :: Subst
identity = \x -> x

single :: VarName -> Term -> Subst

apply :: Subst -> Term -> Term
apply (Subst s) = s

compose :: Subst -> Subst -> Subst