module TagProblemFirstAttempt where

import Prelude hiding(lookup)

data Var = VZ | VS Var
data Exp = V Var | B Bool | L Exp | A (Exp, Exp)

test1 = A (L(V VZ), B True)

lookup (x:env) VZ = x
lookup (x:env) (VS v) = lookup env v

eval0 env (V v)         = lookup env v
eval0 env (B b)         = b
eval0 env (L e)         = \x -> eval0 (x:env) e
eval0 env (A (e1, e2))  = (eval0 env e1) (eval0 env e2)