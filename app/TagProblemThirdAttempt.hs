module TagProblemThirdAttempt where

import Prelude hiding(lookup)

varZ env        = fst env
varS vp env     = vp (snd env)
b (bv::Bool) env = bv
lam e env       = \x -> e (x, env)
app e1 e2 env   = (e1 env) (e2 env)

testf3 = app (lam (varS varZ)) (b True)