module Substitutionen where
import Term

data Subst = Term Term Subst | Subst Term

single :: VarName -> Term -> Subst
