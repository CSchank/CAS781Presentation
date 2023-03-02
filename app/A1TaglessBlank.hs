module A1TagfulDemo where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Prelude hiding(const, sqrt)
import qualified Prelude (sqrt)

-- 1. Add the type signatures for the following methods
class Exprs repr where
    const  :: Double -> repr a
    var    :: String -> repr a
    sqrt   :: repr a -> repr a
    -- uncomment and fill in these type signatures
    intPow :: repr a -> Int -> repr a
    exp    :: repr a -> repr a
    ln     :: repr a -> repr a
    add    :: repr a -> repr a -> repr a
    mult   :: repr a -> repr a -> repr a
    neg    :: repr a -> repr a

-- uncomment this after completing Q1
test1 :: Exprs repr => repr a
test1 = add (var "x") (var "y")


newtype Pretty a = Pretty String

-- 2. Implement the pretty function to unwrap pretty values
pretty :: Pretty a -> String
pretty (Pretty s) = s

-- 3. Implement the Pretty instances of Exprs
instance Exprs Pretty where
    const f    = Pretty $ show f
    var   s    = Pretty s
    sqrt  e    = Pretty $ "sqrt(" ++ pretty e ++ ")"
    intPow e n = Pretty $ pretty e ++ "^" ++ show n
    exp   e    = Pretty $ "e^(" ++ pretty e ++ ")"
    ln    e    = Pretty $ "ln(" ++ pretty e ++ ")"
    add e1 e2  = Pretty $ pretty e1 ++ " + " ++ pretty e2
    mult e1 e2 = Pretty $ pretty e1 ++ " * " ++ pretty e2
    neg   e    = Pretty $ "- (" ++ pretty e ++ ")"

-- uncomment and run this after completing Q3
testQ3 = pretty test1 == "x + y"

newtype Eval a = Eval (Map String Double -> Either String Double)

-- 4. Implement the evaluation function for Exprs
eval :: Map String Double -> Eval a -> Either String Double
eval env (Eval exp) = exp env

testEnv = Map.fromList [("y", 2.5)]

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
    intPow e n = Eval $ \env ->
        case eval env e of
            Right e -> 
                Right $ e ^^ n
            Left err -> Left $ "Error in intPow argument: " ++ err
    exp e = Eval $ \env ->
        case eval env e of
            Right e -> 
                Right $ Prelude.exp e
            Left err -> Left $ "Error in exp argument: " ++ err
    ln e = Eval $ \env ->
        case eval env e of
            Right e -> 
                Right $ Prelude.log e
            Left err -> Left $ "Error in ln argument: " ++ err
    add e1 e2 = Eval $ \env ->
        case (eval env e1, eval env e2) of
            (Right val1, Right val2) -> Right $ val1 + val2
            (Left e, _)              -> Left e
            (_,Left e)               -> Left e
    mult e1 e2 = Eval $ \env ->
        case (eval env e1, eval env e2) of
            (Right val1, Right val2) -> Right $ val1 * val2
            (Left e, _)              -> Left e
            (_,Left e)               -> Left e
    neg e = Eval $ \env ->
        case eval env e of
            Right e -> 
                Right $ -e
            Left err -> Left $ "Error in neg argument: " ++ err

-- uncomment and run this after completing Q5 (assumes you've done Q1!)
testQ5 = eval testEnv test1 == Right 3.5

-- 6. Create an instance to count the number of constructors by following the steps here:

-- Step 1: create a newtype called CC

-- Step 2: create an unwrapped function called cC

-- Step 3: create the instance of Exprs needed


-- Step 4: uncomment this to test it when you're done!
-- testQ6 = cC test1 == 3