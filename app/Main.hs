module Main (main) where

-- https://stackoverflow.com/questions/11492976/creating-an-interactive-program-in-haskell

import System.Environment
import Flow ((|>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Parser.PrimitiveBlock as PrimitiveBlock
import Parser.Language(Language(..))
import qualified Parser.ExprBlock
import qualified Scripta
import qualified Render.Block


main :: IO ()
main = 
 Main.compileToHtml


compileToHtml =             
  do
   [fname] <- getArgs
   text <- TIO.readFile fname
   let blocks = Scripta.compile text
   putStrLn $ Render.Block.render blocks
 

parse =
   do
   [fname] <- getArgs
   text <- TIO.readFile fname
   let blocks = Scripta.compile text
   putStrLn "\nBlocks:\n================="
   TIO.putStrLn $ Parser.ExprBlock.displayBlocks blocks
   putStrLn "================="

parsePrimitiveBlocks =
   do
   [fname] <- getArgs
   text <- TIO.readFile fname
   let blocks = PrimitiveBlock.parse L0Lang (\_ -> True) (T.lines text )  |> filter (\b -> PrimitiveBlock.content b /= [T.pack ""])
   putStrLn "\nPrimitive blocks:\n================="
   TIO.putStrLn $ displayBlocks blocks
   putStrLn "================="


-- putStr "\nEnter a string: "
-- hFlush stdout
