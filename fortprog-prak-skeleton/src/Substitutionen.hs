module Substitutionen where
import Term

data Subst a = Subst a

single :: VarName -> Term -> Subst
