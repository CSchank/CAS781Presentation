module TagProblemTypeError where

import Prelude hiding(lookup)

data Var = VZ | VS Var
data Exp = V Var | B Bool | L Exp | A (Exp, Exp)

data U = UA (U -> U) | UB Bool

test1 = A (L(V VZ), B True)

lookup (x:env) VZ = x
lookup (x:env) (VS v) = lookup env v

eval env (V v)         = lookup env v
eval env (B b)         = UB b
eval env (L e)         = UA $ \x -> eval (x:env) e
eval env (A (e1, e2))  = 
    case eval env e1 of
        UA f -> f (eval env e2)