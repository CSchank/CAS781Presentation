module A1TagfulDemo where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr = Const Float
          | Var String -- in math we often use Char
          | Sqrt Expr -- we can take sqrt(anything)
          | IntPow Expr Int -- the easy case of exponent
          | Exp Expr -- e^expr
          | Ln Expr
          | Mult Expr Expr
          | Add Expr Expr
          | Neg Expr

-- Exp :: Expr -> Expr

pretty :: Expr -> String
pretty (Const f)      = show f
pretty (Var v)        = v
pretty (Sqrt e)       = "sqrt(" ++ pretty e ++ ")"
pretty (IntPow e exp) = pretty e ++ "^" ++ show exp
pretty (Exp e)        = "e^("++ (pretty e) ++ ")"
pretty (Ln e)         = "ln("++pretty e++ ")"
pretty (Add e1 e2)    = (pretty e1) ++ " + " ++ (pretty e2)
pretty (Neg e)        = "- (" ++ pretty e ++ ")"

test1 = Add (Var "x") (Var "y")

e = exp 1

eval :: Map String Float -> Expr -> Either String Float
eval env expr =
  case expr of
    (Var name) -> 
        case Map.lookup name env of
            Just val -> Right val
            Nothing  -> Left ( name ++ " undefined" )
    (Mult expr1 expr2) -> 
      case (eval env expr1, eval env expr2) of
        (Right val1, Right val2) -> Right $ val1 * val2
        (Left e, _)                -> Left e
        (_,Left e)                 -> Left e
    (Sqrt expr1) -> 
      case eval env expr1 of
        Right r -> 
          if r >= 0 then 
            Right $ sqrt r
          else
            Left "Cannot take the square root of a negative number"
        Left e ->
          Left $ "In square root: " ++ e
    (Const c) -> Right c
    (IntPow expr1 exponent) -> 
      fmap (\r -> r ^^ exponent) $ eval env expr1
    (Exp expr1) -> 
      fmap (\r -> exp r) $ eval env expr1
    (Ln expr1) ->  
      fmap (logBase e) $ eval env expr1
    (Add expr1 expr2) -> 
      case (eval env expr1) of
        Right a -> case eval env expr2 of
            Right b -> Right $ a + b
            Left e -> Left e
        Left e -> Left e
    (Neg expr1) ->
      fmap negate $ eval env expr1