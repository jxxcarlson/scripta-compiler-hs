module Main (main) where

-- https://stackoverflow.com/questions/11492976/creating-an-interactive-program-in-haskell

import System.Environment
import TextParser(parseLine)
import Prelude
-- mport Lib

import Control.Monad  
import Data.Char
-- import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import PrimitiveBlock
import Language(Language(..))
import Data.Text.Lazy  (Text, lines, concat, intercalate, length, fromChunks, strip)

hello :: String -> String
hello name = "Hello" ++ " " ++ name ++ "!"

upperCase :: String -> String
upperCase = map toUpper

main :: IO ()
main = 
 do
   [fname] <- getArgs
   text <- TIO.readFile fname
   let blocks = PrimitiveBlock.parse L0Lang (\_ -> True) (Data.Text.Lazy.lines text )
   putStrLn "I did it!"
            

-- parserLoop = 
--   do 
--   putStrLn "\nPress ctrl-C to stop"
--   forever $ do 
--         putStr "\nEnter a string: "
--         hFlush stdout
--         xx <- getLine
--         putStr "  -- "
--         parseLine 4 7  (xx ++ "\n")


-- testLoop = 
--   do 
--   putStrLn "\nPress ctrl-C to stop"
--   forever $ do 
--         putStr "\nEnter a string: "
--         hFlush stdout
--         xs <- getLine
--         putStr "  -- "
--         putStr (show (length xs))
--         putStrLn " characters"