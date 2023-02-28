module A1TagfulDemo where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Prelude hiding(const, sqrt)
import qualified Prelude (sqrt)

-- 1. Add the type signatures for the following methods
class Exprs repr where
    const  :: Double -> repr Double
    var    :: String -> repr String
    sqrt   :: repr Double -> repr Double
    -- uncomment and fill in these type signatures
    -- intPow :: _
    -- exp    :: _
    -- ln     :: _
    -- add    :: _
    -- mult   :: _
    -- neg    :: _

-- uncomment this after completing Q1
--test1 :: Exprs repr => repr a
--test1 = add (var "x") (var "y")


newtype Pretty a = Pretty String

-- 2. Implement the pretty function to unwrap pretty values
pretty :: Pretty a -> String
pretty = error "Not implemented"

-- 3. Implement the Pretty instances of Exprs
instance Exprs Pretty where
    const f    = Pretty $ show f
    var   s    = Pretty s
    sqrt  a    = Pretty $ "sqrt(" ++ pretty a ++ ")"
    -- intPow e n = error "Not implemented"
    -- exp   e    = error "Not implemented"
    -- ln    e    = error "Not implemented"
    -- add e1 e2  = error "Not implemented"
    -- mult e1 e2 = error "Not implemented"
    -- neg   e    = error "Not implemented"

-- uncomment and run this after completing Q3
--testQ3 = pretty test1 == "x + y"

newtype Eval a = Eval (Map String Double -> Either String Double)

-- 4. Implement the evaluation function for Exprs
eval :: Map String Double -> Eval a -> Either String Double
eval env (Eval exp) = error "Not implemented"

testEnv = Map.fromList [("x", 1), ("y", 2.5)]

-- 5. Implement the remaining evaluation functions for the Eval instance
instance Exprs Eval where
    const f = Eval $ \env -> Right f
    var   s = Eval $ \env ->
        case Map.lookup s env of
            Just val -> Right val
            Nothing  -> Left ( s ++ " undefined" )
    sqrt e = Eval $ \env ->
        case eval env e of
            Right e -> 
                if e < 0 then
                    Left $ "Argument to square root is negative"
                else
                    Right $ Prelude.sqrt e
            Left err -> Left $ "Error in square root argument: " ++ err
    -- intPow e n = error "Not implemented"
    -- exp e = error "Not implemented"
    -- ln e = error "Not implemented"
    -- add e1 e2 = error "Not implemented"
    -- mult e1 e2 = error "Not implemented"
    -- neg e = error "Not implemented"

-- uncomment and run this after completing Q5 (assumes you've done Q1!)
-- testQ5 = eval testEnv test1 - 3.5 < 0.00001

-- 6. Create an instance to count the number of constructors by following the steps here:

-- Step 1: create a newtype called CC

-- Step 2: create an unwrapped function called cC

-- Step 3: create the instance of Exprs needed


-- Step 4: uncomment this to test it when you're done!
-- testQ6 = cC test1 == 3