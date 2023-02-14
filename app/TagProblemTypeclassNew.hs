module TagProblemTypeclassNew where

class Sym repr where
    b    :: Bool -> repr Bool
    lam  :: (repr a -> repr b) -> repr (a -> b)
    app  :: repr (a -> b) -> repr a -> repr b

newtype Eval a = Eval a deriving Show
eval (Eval x) = x

-- Eval is not a tag, these will be optimized away by the compiler
instance Sym Eval where
    b bv                    = Eval bv
    lam f                   = Eval $ \x -> eval $ f $ Eval x
    app (Eval e1) (Eval e2) = Eval $ e1 e2

test :: Sym repr => repr Bool
test = app (lam (\x -> x)) (b True)

testEval = eval test

newtype CF a = CF Int deriving Show
cF (CF x) = x

instance Sym CF where
    b _   = CF 0
    lam f = CF $ 1 + cF (f (CF 0))
    app e1 e2 = CF $ (cF e1) + (cF e2)

testCf = cF test

test2 :: Sym repr => repr Bool
test2 = app (lam (\x -> app (lam (\x -> x)) x)) (b True)

testcF2 = cF test2