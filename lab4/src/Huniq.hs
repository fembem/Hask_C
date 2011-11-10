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
    let (filteredLines, _) = foldr (\x acc ->
          let (list, last) = acc in
              if x == last then (list, x) else (x:list, x))
              ([] , "" ) fileLines
    {-let (filteredLines, _) = foldr helperFunc ([] , "" ) fileLines-}
    putStr $ unlines filteredLines
    hClose handle
    return ()
    )
  return ()

helperFunc ::  String -> ([String], String)   -> ([String], String)
helperFunc x accum  =
  let (list, last) = accum
  in if x == last then (list, x) else (x:list, x)

{-testing-}
{-a-}
{-a-}
{-b-}
{-b-}
{-b-}
{-c-}
{-b-}
{-c-}
{-c-}
