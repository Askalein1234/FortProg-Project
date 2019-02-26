module Substitutionen where
import Term

data Subst = Subst Term -> Term

single :: VarName -> Term -> Subst
