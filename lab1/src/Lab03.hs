-----------------------------------------------------------------------------
--
-- Module      :  Lab03
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------


module Lab03 (
    main
)
where

import Test.QuickCheck

main = do putStrLn "CS11 Lab 02"
          quickCheck (prop_facts_agree :: AbstractInteger -> Bool)

instance Arbitrary AbstractInteger where
    arbitrary = do
                    n <- choose (0,7):: Gen Integer
                    return (integer_toAI n)

prop_facts_agree x = factorial x == factorialTail x

data AbstractInteger = Zero | Succ AbstractInteger | Pred AbstractInteger
    deriving (Show)

ai_toInteger :: AbstractInteger -> Integer
ai_toInteger Zero = 0
ai_toInteger (Succ a) = 1 + ai_toInteger a
ai_toInteger (Pred a) = -1 + ai_toInteger a

integer_toAI :: Integer -> AbstractInteger
integer_toAI x  |  x >  0        =   Succ (integer_toAI (x-1) )
                |  x == 0        =   Zero
                |  x <  0        =   Pred (integer_toAI (x+1) )

abstractIntegerEq :: AbstractInteger -> AbstractInteger -> Bool
abstractIntegerEq Zero Zero = True
abstractIntegerEq (Succ a) (Succ b) = abstractIntegerEq a b
abstractIntegerEq (Pred a) (Pred b) = abstractIntegerEq a b
abstractIntegerEq _ _ = False

instance Eq AbstractInteger where
    x == y =  x `abstractIntegerEq` y

instance Ord AbstractInteger where
    x <= y               = case (x,y) of
                            (Zero, Zero)      ->  True
                            (Zero, Succ _)    ->  True
                            (Pred _, Zero)    ->  True
                            (Pred _, Succ _)  ->  True
                            (Pred a, Pred b)  ->  a <= b
                            (Succ a, Succ b)  ->  a <= b
                            (_,_)             -> False

instance Num AbstractInteger where
    a + b = case (a,b) of
                            (Pred c, Succ d)    ->  c + d
                            (Succ c, Pred d)    ->  c + d
                            (Succ c, Succ d)    ->  c + Succ ( Succ d)
                            (Pred c, Pred d)    ->  c + Pred ( Pred d)
                            (c, Zero)    ->  c
                            (Zero, c)    ->  c
    negate a = case a of
                            Pred c    ->  Succ (negate c)
                            Succ c    ->  Pred (negate c)
                            Zero      ->  Zero
    abs a = case a of
                            Pred c    ->  Succ (negate c)
                            nonNegative    ->  nonNegative
    a * b = case (a,b) of
                            (Zero, _)    ->  Zero  {- redundant, for efficiency -}
                            (_, Zero)    ->  Zero
                            (c, Succ d)    ->  c + (c * d)
                            (c, Pred d)    ->  (negate c) + (c * d)
    fromInteger = integer_toAI

myHead :: [a] -> a
myHead (x:xs)             =  x
myHead  []                =  error "head{PreludeList}: head []"

factorial :: (Ord a, Num a) => a -> a
factorial x  |  x >  0        =   x * factorial(x-1)
             |  x == 0        =   1
             | True           =   error "factorial of negative number!"

factorialTail :: (Ord a, Num a) => a -> a
factorialTail x = factorialTailHelper x 1
factorialTailHelper x  accum    |  x >  0        =   factorialTailHelper (x-1) (x*accum)
                                |  x == 0        =   accum
                                |  True          =   error "factorial of negative number!"


