module TagProblemThirdAttempt where

import Prelude hiding(lookup)

b (bv::Bool) = bv
lam e        = \x -> e x
app e1 e2    = e1 e2

testf3 = app (lam (\x -> x)) (b True)