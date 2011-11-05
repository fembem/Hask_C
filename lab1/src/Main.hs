-----------------------------------------------------------------------------
--
-- Module      :  CS11 Lab 01
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :    Leo deCandia
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (

    main
) where

main = do putStrLn "CS11 Lab 01"
          putStrLn ( "add_and_double 3 4: " ++ show (add_and_double 3 4))
          putStrLn ( "(+*) 3 4: " ++ show (3 +* 4))
          putStrLn ( "solve_quadratic_equation 1 2 1: "
            ++ show (solve_quadratic_equation 1 2 1))
          putStrLn ( "first_n 10: " ++ show (first_n 10))
          putStrLn ( "first_n_integers 10: " ++ show (first_n_integers 10))
          putStrLn ( "double_factorial 3: " ++ show (double_factorial 3))
          putStrLn ( "take 5 factorialZip 10: " ++ show (take 5 factorialZip))


{-
main = putStrLn "Hello World!"
-}

{-
add                     :: Integer -> Integer -> Integer
add x y                 = x + y
-}
add_and_double          :: Double -> Double -> Double
add_and_double x y      = 2 * (x + y)

(+*)                    :: Double -> Double -> Double
x +* y                 = add_and_double x y

solve_quadratic_equation          :: Double -> Double -> Double ->
                                        (Double, Double)
solve_quadratic_equation a b c    = (term1 + term2, term1 - term2)
    where term1 = -b/(2 * a)
          term2 = disc/(2 * a)
          disc = sqrt(b * b - 4 * a * c)

first_n :: Int -> [Int]
first_n n = take n [1..]

first_n_integers :: Integer -> [Integer]
first_n_integers n = take_integer n 1 []
    where take_integer n m integers
            | n > 0 = m: take_integer (n - 1) (m + 1) integers
            | otherwise = integers
{-
factorial2 :: Integer -> Integer
factorial2 n   | n < 0 = error "argument cannot be negative"
               | n <= 1 = 1
               | otherwise = n * factorial2(n-1)
-}

double_factorial :: Integer -> Integer
double_factorial n | n < 0 = error "argument cannot be negative"
                   | n <= 1 = 1
                   | otherwise = double_factorial(n-1) * factorial(n)
    where factorial n   | n < 0 = error "argument cannot be negative"
                        | n <= 1 = 1
                        | otherwise = n * factorial(n-1)
{-
ackermann m n | m < 0 || n < 0 = error "arguments cannot be negative"
                   | m == 0 = n + 1
                   | m > 0 && n == 0 = ackermann (m-1) 1
                   | otherwise = ackermann (m-1) (ackermann m (n-1))

fib             = 1 : 1 : [ a + b | (a, b) <- zip fib (tail fib) ]
-}

factorialZip :: [Integer]
factorialZip = 1 : 1 : [ x | x <- zipWith ( * ) [2..] (tail factorialZip)]
