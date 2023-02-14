module TagProblemTypeclassCPS where

class Sym repr where
    b    :: Bool -> repr Bool
    lam  :: (repr a -> repr b) -> repr (a -> b)
    app  :: repr (a -> b) -> repr a -> repr b

data CPS k = CPS { ko :: forall a . (k -> a) -> a }
run a = (ko a) (\x -> x)

-- Eval is not a tag, these will be optimized away by the compiler
instance Sym CPS where
    b bv      = CPS $ \k -> bv
    lam f     = CPS $ \k -> k f
    app e1 e2 = CPS $ \k -> e1 (\f -> f e2 k)