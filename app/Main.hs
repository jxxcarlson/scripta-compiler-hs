module Main (main) where

-- https://stackoverflow.com/questions/11492976/creating-an-interactive-program-in-haskell

import System.Environment
import Flow ((|>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List

import Compiler.Parser.PrimitiveBlock as PrimitiveBlock
import Compiler.Parser.Language(Language(..))
import qualified Compiler.Parser.ExprBlock as Parser.ExprBlock
import qualified Compiler.Scripta as Scripta
import qualified Compiler.Render.Block as Render.Block


main :: IO ()
main = 
  do 
    [prog, fname] <- getArgs
    case prog of 
      "html" ->  Main.compileToHtml fname
      "pb"   ->  Main.parsePrimitiveBlocks fname
      "expr" -> Main.parse fname
      _ -> putStrLn "unknown subcommand"


compileToHtml fname =             
  do 
   text <- TIO.readFile fname
   let blocks = Scripta.compile text
   putStrLn $ Render.Block.renderToString blocks

parse fname =
   do
   text <- TIO.readFile fname
   let blocks = Scripta.compile text
   putStrLn "\nBlocks:\n================="
   TIO.putStrLn $ Parser.ExprBlock.displayBlocks blocks
   putStrLn "================="

parsePrimitiveBlocks fname =
   do
   text <- TIO.readFile fname
   let blocks = PrimitiveBlock.parse L0Lang (\_ -> True) (T.lines text )  |> filter (\b -> PrimitiveBlock.content b /= [T.pack ""])
   putStrLn "\nPrimitive blocks:\n================="
   TIO.putStrLn $ displayBlocks blocks
   putStrLn "================="


-- putStr "\nEnter a string: "
-- hFlush stdout
