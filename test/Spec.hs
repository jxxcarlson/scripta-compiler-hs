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


main :: IO ()
main = hspec $ do
  describe "PrimitiveBlock.parse" $ do
    it "can parse an ordinary block" $ do
      (parseBlock input1 |> PB.displayBlocks) `shouldBe` expected1

    it "can parse a paragraph" $ do
      (parseBlock input2 |> PB.displayBlocks) `shouldBe` expected2

parseBlock :: Text -> [PrimitiveBlock]
parseBlock text = PB.parse L0Lang (\_ -> True) (T.lines text ) 

input1 = "| section 1 foo:bar yada:a b c \n\
\ Introduction"

expected1 = T.pack "type: Ordinary\n\
\name: section\n\
\args: 1\n\
\dict: foo: bar yada: a b c\n\
\------\n\
\Introduction\n\n"


input2 = "abc\n\
\def"

expected2 = "type: Paragraph\n\
\name: anon\n\
\args:\n\
\dict:\n\
\------\n\
\abc\n\
\def\n\n"

-- |> filter (\b -> PrimitiveBlock.content b /= [T.pack ""])