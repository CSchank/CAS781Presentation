module EqTypeclassExample where

import Prelude hiding(Eq,(==))

class Eq a where
    (==) :: a -> a -> Bool

data Colour = Red | Yellow | Green

instance Eq Colour where
    (==) Red Red = True
    (==) Yellow Yellow = True
    (==) Green Green = True
    (==) a b = False

testT = Red == Red
testF = Red == Yellow

-- bad type: fancyEq :: a -> a -> String
fancyEq :: Eq a => a -> a -> String
fancyEq x y = 
    if x == y then "They're equal!"
    else "They're not equal :("