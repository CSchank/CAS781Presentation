module TagProblemTypeclass where

class Symantiqs repr where
    varZ :: repr (a,b) -> repr a
    varS :: repr (a -> b) -> repr (c, a) -> repr b
    b    :: Bool -> repr (a, b) -> repr Bool
    lam  :: (repr (a, env) -> repr b) -> repr env -> repr (a -> b)
    app  :: repr (env -> a -> b) -> repr (env -> a) -> repr env -> repr b
    
    -- varZ env        = fst env
    -- varS vp env     = vp (snd env)
    -- b (bv::Bool) env = bv
    -- lam e env       = \x -> e (x, env)
    -- app e1 e2 env   = (e1 env) (e2 env)

newtype R a = R a deriving Show
unR (R x) = x

-- R is not a tag, these will be optimized away by the compiler
instance Symantiqs R where
    varZ (R env) = R $ fst env
    varS (R f) (R env) = R $ f (snd env)
    b bv (R env) = R bv
    lam f (R env) = R $ \x -> unR $ f $ R (x,env)
    app (R e1) (R e2) (R env) = R $ (e1 env) (e2 env)

testf3 = app (R $ lam varZ) (b True)