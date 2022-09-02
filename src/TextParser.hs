

{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module TextParser (parseLine,lineParser, Line1) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Flow ((|>))
import Data.Text.Lazy as Text ( pack, Text, take, strip, words, drop )
import System.Environment


type Parser = Parsec Data.Void.Void Text

data Line1 =
    Line1 { indent :: Int, prefix :: Text, content :: Text, lineNumber :: Int, position :: Int } deriving(Show)


data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)


lineParser :: Int -> Int -> Parser Line1
lineParser position lineNumber = 
  do 
    prefix <- many (satisfy (\c -> c == ' ')) 
    content <- many (satisfy (\c -> c /= '\n')) 
    rawContent <- getInput
    return Line1 {indent =  length prefix, prefix = pack prefix, position = position, lineNumber = lineNumber, content = pack content}

slice :: Int -> Int -> [a] -> [a]
slice from to xs = Prelude.take (to - from + 1) (Prelude.drop from xs)


parseLine position lineNumber str = parseTest (lineParser position lineNumber) (pack str)