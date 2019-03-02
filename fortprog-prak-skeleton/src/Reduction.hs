module Reduction where

import Prog
import Matching

findRule :: Prog -> Term -> Maybe (Rhs, Subst)
