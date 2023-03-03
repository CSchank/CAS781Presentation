{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, 
MultiParamTypeClasses, TypeApplications, InstanceSigs,
FlexibleContexts, UndecidableInstances, AllowAmbiguousTypes,
FlexibleInstances, IncoherentInstances, PartialTypeSignatures
#-}

module TypeLevelMapsTest where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Prelude hiding(const, sqrt)
import qualified Prelude (sqrt)

import Data.Type.Map as TM

import GHC.TypeLits

foo :: TM.Map '["x" :-> Int, "z" :-> Bool, "w" :-> Int]
foo = Ext (Var @"x") 2 $ Ext (Var @"z") True $ Ext (Var @"w") 5 $ Empty

x :: Int
x = lookp (Var @"x") foo

class Exprs repr where
    constD  :: Double  -> repr '[] a
    constI  :: Int  -> repr '[] a
    varD   :: IsMember v Double env => Var v -> repr env a
    varI   :: IsMember v Integer env => Var v -> repr env a
    sqrt   :: repr env a -> repr env b
    add    :: (Submap e1 env, Submap e2 env) => repr e1 a -> repr e2 a -> repr env b

data Eval r env a = Eval (TM.Map (env :: [Mapping Symbol *]) -> r)

eval env (Eval expr) = expr env

instance Exprs (Eval Double) where
    constD d = Eval $ \_ -> d
    constI i = Eval $ \_ -> fromIntegral i

   -- var :: IsMember v Double env => Var v -> Eval env a
    varD v   = Eval $ (\env -> lookp v env) 
    varI v   = Eval $ (\env -> fromIntegral $ lookp v env) 

    sqrt e = Eval $ \env -> Prelude.sqrt $ eval env e

    add e1 e2 = Eval $ \env -> (eval (submap env) e1) + (eval (submap env) e2)

test :: _ => repr '["x" :-> Double, "y" :-> Integer] a
test = add (varI (Var @"y")) (sqrt (varD (Var @"x")))

testEnv = Ext (Var @"x") 2.0 $ Ext (Var @"y") 2 $ Empty

testResult :: Double
testResult = eval testEnv test