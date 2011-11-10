-----------------------------------------------------------------------------
--
-- Module      :  Hello3
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

import System( getArgs )

main = do
  args <- getArgs
  print $ show args
  mapM_ print args


