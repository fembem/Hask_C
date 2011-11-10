-----------------------------------------------------------------------------
--
-- Module      :  Hsort
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

module Huniq (
  main
) where

import System.Environment
import System.IO
import Data.List
import Control.Monad

main = do
  args <- getArgs
  forM_ args (\fileName -> do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    let filteredLines = foldl1 helperFunc ([] , () ) fileLines
    putStr $ unlines filteredLines
    hClose handle
    return ()
    )
  return ()

helperFunc :: ([String], String) -> String -> ([String], String)
helperFunc accum x =
  let (list, last) = accum
  in if x == last then (list, x) else (x : list, x)


{- code from http://learnyouahaskell.com/recursion -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

{-

main = do
  args <- getArgs
  forM_ args (\fileName -> do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    let filteredLines = foldl1 (\ accum x ->
                  let (list, last) = accum in
                    if x == last then (list, x) else (x : list, x) )
                  ([] , () )
                  fileLines
    putStr $ unlines filteredLines
    hClose handle
    return ()
    )
  return ()

-}
