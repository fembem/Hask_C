-----------------------------------------------------------------------------
--
-- Module      :  Lab02
-- Author      : Leo deCandia
-- |
--
-----------------------------------------------------------------------------

module Lab02 (
    main
) where

main = do putStrLn "CS11 Lab 02"
          putStrLn ( "sumIntegers1 [1, 2, 3, 4]: "
            ++ show (sumIntegers1 [1, 2, 3, 4] ) )
          putStrLn ( "sumIntegers2 [1, 2, 3, 4]: "
            ++ show (sumIntegers2 [1, 2, 3, 4] ) )
          putStrLn ( "prodIntegers [1, 2, 3, 4]: "
            ++ show (prodIntegers [1, 2, 3, 4] ) )
          putStrLn ( "listAppend [1, 2, 3, 4] [5, 6, 7, 8]: "
            ++ show (listAppend [1, 2, 3, 4] [5, 6, 7, 8] ) )
          putStrLn ( "insertionSort [3, 7, 4, 5, 2, 8, 1]: "
            ++ show (insertionSort [3, 7, 4, 5, 2, 8, 1] ) )
          putStrLn ( "map2 (*) [1, 2, 3, 4, 5] [5, 4, 3, 2, 1]: "
            ++ show ( map2 (*) [1, 2, 3, 4, 5] [5, 4, 3, 2, 1] ) )
          putStrLn ( "take 10 factorials: "
            ++ show ( take 10 factorials ) )
          -- putStrLn ( "take 10 primes: "
          --   ++ show ( take 10 primes ) )
          putStrLn ( "countLeaves sampleTree: "
            ++ show ( countLeaves sampleTree ) )
          putStrLn ( "mapTree (*2) sampleTree: "
            ++ show ( mapTree (*2) sampleTree ) )
          putStrLn ( "charsInTree: "
            ++ show ( charsInTree ) )
-- lab2.hs
--

-- Sum a list of integers recursively.
sumIntegers1 :: [Integer] -> Integer
sumIntegers1 [] = 0
sumIntegers1 (x:xs) = x + sumIntegers1 xs

-- Sum a list of integers in terms of foldl.
sumIntegers2 :: [Integer] -> Integer
sumIntegers2 = foldl (+) 0

-- Product of a list of integers in terms of foldr.
prodIntegers :: [Integer] -> Integer
prodIntegers = foldr (*) 1

-- List append in terms of foldr.
listAppend :: [a] -> [a] -> [a]
listAppend x y = foldr (:) y x

-- Insertion sort.
-- This sorts a list of values (which all have to be of type class Ord)
-- according to the following algorithm:
-- 1) Sort the sublist consisting of the tail of the list.
-- 2) Insert the first element (the head) into the sorted sublist at the
--    correct place.
-- Sort the list in ascending order.
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = [y | y <- sorted, y < x] ++ [x]
                        ++ [y | y <- sorted, y >= x]
                     where sorted = insertionSort xs

-- bookzip :: [a] -> [b] -> [(a, b)]
-- bookzip (x:xs) (y:ys)       = (x, y) : bookzip xs ys
-- bookzip  xs     ys          = []

-- 'map' that works on two lists.
-- map f a b = [(f a1 b1), (f a2 b2), ...]
--     where a = [a1, a2, ...] and b = [b1, b2, ...]
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 op (x:xs) (y:ys)       = (op x y) : ( map2 op xs ys )
map2  _  _ _          = []

-- Infinite list of factorials, starting from 0!, using map2.
factorials :: [Integer]
factorials = 1: ( map2 (*) [1..] factorials )

-- Infinite list of prime numbers using list comprehensions and a sieving
-- algorithm. The sieve works as follows: it's n followed by (the sieve of)
-- all numbers that aren't divisible by n.
-- primes :: [Integer]
-- primes = sieve 2
--     where sieve n = n: sieve [y | y <- [(n+1)..] , y > n && y `rem` n > 0]

-- unbounded sieve, premature filters
primesT () = 2 : sieve [3,5..]  where
    sieve (p:xs) = p : sieve [x | x<-xs, rem x p /= 0]
                         -- filter ((/=0).(`rem`p)) xs

-- Tree data structure.  Note that this is different from the Tree
-- structure used in the lecture.
data Tree a = Leaf | Node a (Tree a) (Tree a)
              deriving Show

-- Sample tree (for testing).
sampleTree :: Tree Integer
sampleTree = Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 Leaf Leaf)

-- Count the number of leaves in a Tree.
countLeaves :: Show a => Tree a -> Integer
countLeaves Leaf = 1
countLeaves (Node _ l r) = countLeaves l + countLeaves r

-- Mapping over a tree.
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Leaf = Leaf
mapTree fun (Node item l r) = Node (fun item) (mapTree fun l) (mapTree fun r)


-- Some data/functions for testing:

-- Counting characters in a String.
countChars :: String -> Integer
countChars = toInteger . length

-- Sample tree of strings.
treeOfStrings :: Tree String
treeOfStrings = Node "foo"
                (Node "bar" (Node "baz" Leaf Leaf) Leaf)
                (Node "bam" Leaf Leaf)

treeOfNums :: Tree Integer
treeOfNums = mapTree countChars treeOfStrings

-- Fold on a tree.
foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree op accum Leaf = accum
foldTree op accum (Node item l r) =
    let lfold = foldTree op (op accum item) l
    in foldTree op lfold r

-- Sample use of foldTree: count the characters in a tree of Strings.
charsInTree :: Integer
charsInTree = foldTree (+) 0 (mapTree countChars treeOfStrings)


--
-- end of lab2.hs
--

