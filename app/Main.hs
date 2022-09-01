module Main (main) where

-- https://stackoverflow.com/questions/11492976/creating-an-interactive-program-in-haskell

import System.Environment
import MyParser (singleLetterP, ptest)
import Prelude
-- mport Lib

import Control.Monad  
import Data.Char
import System.IO

hello :: String -> String
hello name = "Hello" ++ " " ++ name ++ "!"

upperCase :: String -> String
upperCase = map toUpper

main :: IO ()
main = 
    do 
    putStrLn "Press ctrl-C to stop"
    forever $ do 
        putStr "\nEnter a string: "
        hFlush stdout
        xs <- getLine
        putStr "  -- "
        putStr (show (length xs))
        putStrLn " characters"
            