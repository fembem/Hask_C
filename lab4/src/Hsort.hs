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

module Main (
  main
) where

import System.Environment
import System.IO
import Control.Monad

main = do
  args <- getArgs
  forM_ args (\fileName -> do
    handle <- openFile fileName ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    putStr $ unlines $ quicksort fileLines
    hClose handle
    return ()
    )
  return ()

{- code from http://learnyouahaskell.com/recursion -}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

