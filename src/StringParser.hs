{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module StringParser (parseString, lineParser, textParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Flow ((|>))
import Data.Text.Lazy as Text ( pack, Text, take, strip, words, drop )
import System.Environment

data Line1 =
    Line1 { indent :: Int, prefix :: String, content :: String, lineNumber :: Int, position :: Int } deriving Show


type Parser = Parsec Data.Void.Void String

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)


-- singleLetterP :: Parser Char
-- singleLetterP = char 'h'

-- bool :: Parser Bool
-- bool = False <$ string "false" <|> True <$ string "true"



-- ptest str = parseTest bool str

--lineParser :: Int -> Int -> Parser Line
-- lineParser position lineNumber =
--     -- Parser.succeed (\prefixStart prefixEnd lineEnd content
--     -- -> {   indent = prefixEnd - prefixStart
--     --      , prefix = String.slice 0 prefixEnd content
--     --      , content = String.slice prefixEnd lineEnd content
--     --      , position = position
--     --      , lineNumber = lineNumber }
--     --    )
--       do 
--         a <- getOffset
--          some (\c -> c == ' ')
--         <*> getOffset
--         <*> some (\c -> c /= '\n')
--         <*>  getOffset
--         <*>  getInput


lineParser :: Int -> Int -> Parser Line1
lineParser position lineNumber = 
  do 
    --a <- getOffset
    prefix <- many (satisfy (\c -> c == ' ')) -- some (\c -> c == ' ')
    -- b <- getOffset
    content <- many (satisfy (\c -> c /= '\n')) -- some (\c -> c /= '\n')
    -- c <- getOffset
    rawContent <- getInput
    --let prefix = slice a b rawContent
    return Line1 {indent =  length prefix, prefix = prefix, position = position, lineNumber = lineNumber, content = content}

slice :: Int -> Int -> [a] -> [a]
slice from to xs = Prelude.take (to - from + 1) (Prelude.drop from xs)

lineParser_ :: Parser String
lineParser_ = 
  many (satisfy (\c -> c /= '\n'))

eolP :: Parser Char
eolP = satisfy (\c -> c == '\n') 


first :: Parser a -> Parser b -> Parser a
first pa pb = do
    a <- pa
    b <- pb
    return a

simpleLineParser = first lineParser_ eolP

textParser = many simpleLineParser


-- *TxtParser> test "abc\ndef\nghi\n"
-- ["abc","def","ghi"]
test str = parseTest simpleLineParser str

-- xtest 4 13 "   || foo bar baz\n"
-- Line1 {indent = 3, prefix = "   ", content = "|| foo bar baz", lineNumber = 13, position = 4}
parseString aa bb str = parseTest (lineParser aa bb) str