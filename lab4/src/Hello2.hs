-----------------------------------------------------------------------------
--
-- Module      :  Hello2
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

main = do
    putStrLn "Please enter your name: "
    name <- getLine
    putStrLn ("hello, " ++ name )

