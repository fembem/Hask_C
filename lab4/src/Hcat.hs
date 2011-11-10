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
  if (length args /= 0) then do
    forM_ args (\fileName -> do
      handle <- openFile fileName ReadMode
      contents <- hGetContents handle
      let fileLines = lines contents
      putStr $ unlines fileLines
      hClose handle
      return ()
      )
    return ()
  else do
    contents <- getContents
    putStr contents

