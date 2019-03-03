module Evaluation where

import Prog
import Term
import Substitutionen
import Matching
import Helper
import Positionen
import Reduction
import Data.List

-- Alias type for evaluation strategies.
type Strategy = Prog -> Term -> [Pos]

-- loStrategy :: Strategy
-- loStrategy = \p t -> 