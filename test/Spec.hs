{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
-- import Test.QuickCheck
-- import Control.Exception (evaluate)
import PrimitiveBlock(PrimitiveBlock)
import qualified PrimitiveBlock as PB
import Data.Text (Text)
import qualified Data.Text as T
import Language (Language(..))
import Flow ((|>))

-- import System.Environment
import qualified Data.Text.IO as TIO


main :: IO ()
main = hspec $ do
  describe "PrimitiveBlock.parse" $ do
    it "can parse an ordinary block" $ do
      (parseBlock input1 |> PB.displayBlocks) `shouldBe` expected1

    it "can parse a paragraph" $ do
      (parseBlock input2 |> PB.displayBlocks) `shouldBe` expected2

    it "can compute character positions, 0" $ checkPosition 0

    it "can compute character positions, 2" $ checkPosition 2

    it "can compute character positions, 4" $ checkPosition 4

    it "can compute character positions, 6"$ checkPosition 6



parseBlock :: Text -> [PrimitiveBlock]
parseBlock text = PB.parse L0Lang (\_ -> True) (T.lines text ) 

input1 = "| section 1 foo:bar yada:a b c \n\
\ Introduction"

expected1 = T.pack "type: Ordinary\n\
\lineNumber: 1\n\
\position: 0\n\
\indent: 0\n\
\name: section\n\
\args:, 1\n\
\dict: foo: bar, yada: a b c\n\
\------\n\
\Introduction\n\n"


input2 = "abc\n\
\def"

expected2 = "type: Paragraph\n\
\lineNumber: 1\n\
\position: 0\n\
\indent: 0\n\
\name: anon\n\
\args:\n\
\dict: \n\
\------\n\
\abc\n\
\def\n\n"



checkPosition blockNumber = 
    do
    text <- TIO.readFile "ex1.txt"
    let lines = T.lines text
    let blocks = parseBlock text
    let block = blocks !! blockNumber
    let lineNo = (PB.lineNumber block)
    let firstLine = lines !! (lineNo - 1)
=   let pos = PB.position block
    firstLine `shouldBe` (slice (pos) (pos + T.length firstLine - 1) text)

slice :: Int -> Int -> Text -> Text
slice a b text = 
    text |> T.take (b + 1) |> T.drop a