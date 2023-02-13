module TagProblemSecondAttempt where

import Prelude hiding(lookup)

data Var = VZ | VS Var
    deriving(Show)
data Exp = V Var | B Bool | L Exp | A (Exp, Exp)
    deriving(Show)
data U = UA (U -> U) | UB Bool

instance Show U where
    show (UA _) = "UA (fun)"
    show (UB b) = "UB (" ++ show b ++ ")"

test1 = A (L(V VZ), B True)
test3 = A (L (V (VS VZ)), B True)
test3b = A (L (V (VS (VS VZ))), B True)

lookup (x:env) VZ = x
lookup (x:env) (VS v) = lookup env v

eval env (V v)         = lookup env v
eval env (B b)         = UB b
eval env (L e)         = UA $ \x -> eval (x:env) e
eval env (A (e1, e2))  = 
    case eval env e1 of
        UA f -> f (eval env e2)