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

newtype CC a = CC Int deriving Show
cC (CC x) = x

instance Sym CC where
    b _   = CC 1
    lam f = CC $ 1 + cC (f (CC 0))
    app e1 e2 = CC $ 1 + (cC e1) + (cC e2)

newtype CF a = CF Int deriving Show
cF (CF x) = x

testCC = cC test

instance Sym CF where
    b _   = CF 0
    lam f = CF $ 1 + cF (f (CF 0))
    app e1 e2 = CF $ (cF e1) + (cF e2)

testCf = cF test

test2 :: Sym repr => repr Bool
test2 = app (lam (\x -> app (lam (\x -> x)) x)) (b True)

testcF2 = cF test2

-- Extending our language to include integers
class SymInt repr where
    i :: Int -> repr Int
    -- Exercise: extend this to include +, -, * and / on integers

instance SymInt Eval where
    i n = Eval n

instance SymInt CC where
    i n = CC 1

instance SymInt CF where
    i n = CF 0

class (Sym repr, SymInt repr) => SymLang repr
instance SymLang Eval
instance SymLang CC
instance SymLang CF

testInt :: (Sym repr, SymInt repr) => repr Int
testInt = app (lam (\x -> app (lam (\x -> x)) x)) (i 42)

testInt' :: SymLang repr => repr Int
testInt' = app (lam (\x -> app (lam (\x -> x)) x)) (i 42)

testIntEval = eval testInt