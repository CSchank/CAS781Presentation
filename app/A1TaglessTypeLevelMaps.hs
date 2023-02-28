module A1TaglessTypeLevelMaps where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Prelude hiding(const, sqrt)
import qualified Prelude (sqrt)

import qualified Data.Type.Map as TM

class Exprs repr where
    const  :: Double  -> repr Double
    var    :: String -> repr String
    sqrt   :: repr a -> repr b
    intPow :: repr a -> Int -> repr b
    exp    :: repr a -> repr b
    ln     :: repr a -> repr b
    add    :: repr a -> repr a -> repr b
    mult   :: repr a -> repr a -> repr b
    neg    :: repr a -> repr b

newtype Pretty a = Pretty String

pretty (Pretty a) = a

instance Exprs Pretty where
    const f    = Pretty $ show f
    var   s    = Pretty s
    sqrt  a    = Pretty $ "sqrt(" ++ pretty a ++ ")"
    intPow e n = Pretty $ pretty e ++ "^" ++ show n
    exp   e    = Pretty $ "e^(" ++ pretty e ++ ")"
    ln    e    = Pretty $ "ln(" ++ pretty e ++ ")"
    add e1 e2  = Pretty $ pretty e1 ++ " + " ++ pretty e2
    mult e1 e2 = Pretty $ pretty e1 ++ " * " ++ pretty e2
    neg   e    = Pretty $ "- (" ++ pretty e ++ ")"

test1 :: Exprs repr => repr a
test1 = add (var "x") (var "y")

newtype Eval a = Eval (Map String Double -> Either String Double)
eval env (Eval exp) = exp env

testEnv = Map.fromList [("x", 1), ("y", 2.5)]

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