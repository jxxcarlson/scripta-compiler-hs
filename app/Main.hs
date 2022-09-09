module Main (main) where

-- https://stackoverflow.com/questions/11492976/creating-an-interactive-program-in-haskell

import System.Environment
import Flow ((|>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List

import Parser.PrimitiveBlock as PrimitiveBlock
import Parser.Language(Language(..))
import qualified Parser.ExprBlock
import qualified Scripta
import qualified Render.Block


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
   putStrLn $ header <> Render.Block.render blocks
 
header :: String
header = 
   let 
      header1 = "<!DOCTYPE HTML>\n<head>"
      header2 = "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css\" integrity=\"sha384-bYdxxUwYipFNohQlHt0bjN/LCpueqWz13HufFEV1SUatKs1cm4L6fFgCi1jT643X\" crossorigin=\"anonymous\">"
      header3 =  "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js\" integrity=\"sha384-Qsn9KnoKISj6dI8g7p1HBlNpVx0I8p1SvlwOldgi3IorMle61nQy4zEahWYtljaz\" crossorigin=\"anonymous\"></script>"
      header4 = "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js\" integrity=\"sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05\" crossorigin=\"anonymous\" onload=\"renderMathInElement(document.body);\"></script>\n</head>"
      header5 = "<link rel=\"stylesheet\" href=\"style.css\">"

   in 
   [header1, header2, header3, header4, header5] |> Data.List.intercalate "\n\n"

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
