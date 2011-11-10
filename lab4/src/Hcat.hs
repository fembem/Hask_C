-----------------------------------------------------------------------------
--
-- Module      :  Hcat
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
    putStr $ unlines fileLines
    hClose handle
    return ()
    )
  return ()

