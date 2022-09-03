{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Line(PrimitiveBlockType(..), Line, getNameAndArgs, isEmpty, indent, prefix,content, lineNumber, position, classify, getBlockType) where

-- https://markkarpov.com/tutorial/megaparsec.html
-- https://serokell.io/blog/parser-combinators-in-haskell
-- https://akashagrawal.me/2017/01/19/beginners-guide-to-megaparsec.html

--import Data.Text.Lazy as Text ( pack, unpack, Text, take, strip, words, drop )
import Data.Text as Text (Text, take, words, pack, drop, concat, intercalate, length, strip)

import Data.Functor.Identity (Identity)
import Data.Maybe(maybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Flow ((|>))


import Language (Language(..)) 

data Line =
    Line { indent :: Int, prefix :: Text, content :: Text, lineNumber :: Int, position :: Int } deriving(Show)

type LineParser = Parsec Data.Void.Void Text

data PrimitiveBlockType = PBVerbatim | PBOrdinary | PBParagraph deriving (Show, Eq)


classify :: Int -> Int -> Text -> Line
classify position lineNumber txt =
     case parseMaybe (lineParser position lineNumber) txt of 
        Nothing -> Line {indent = 0, prefix = "", content = "", lineNumber = 0, position = 0}
        Just l -> l

isEmpty :: Line -> Bool
isEmpty line =
    indent line == 0 && content line == ""


getBlockType :: Language -> Text -> PrimitiveBlockType
getBlockType lang line_ =
    let
        line =
            Text.strip line_
    in
    case lang of
        L0Lang ->
            if Text.take 2 line == "||" then
                PBVerbatim

            else if Text.take 2 line == "$$" then
                PBVerbatim

            else if
                Text.take 1 line
                    == "|"
            then
                PBOrdinary

            else
                PBParagraph

        MicroLaTeXLang ->
            -- Note the source text has already been partially transformed to conform to L0
            if Text.take 2 line == "||" then
                PBVerbatim

            else if Text.take 2 line == "$$" then
                PBVerbatim

            else if
                Text.take 1 line
                    == "|"
            then
                PBOrdinary

            else
                PBParagraph


getNameAndArgs :: Line -> (Maybe Text, [Text])
getNameAndArgs line =
    let
        normalizedLine =
            Text.strip (content line)

        -- account for possible indentation
    in
    if Text.take 2 normalizedLine == "||" then
        let
            words =
                Text.words (Text.drop 3 normalizedLine)

            name =
                 head_ words

            args =
                Prelude.drop 1 words
        in
        (  name, args )

    else if Text.take 1 normalizedLine == "|" then
        let
            words =
                Text.words (Text.drop 2 normalizedLine)

            name =
                head_ words 

            args =
                Prelude.drop 1 words
        in
        ( name, args )

    else if Text.take 2 (content line) == "$$" then
        ( Just "math", [] )

    else
        ( Nothing, [] )


head_ :: [a] -> Maybe a 
head_ [] = Nothing 
head_ (first:rest) = Just first


data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)


lineParser :: Int -> Int -> LineParser Line
lineParser position lineNumber = 
  do 
    prefix <- many (satisfy (\c -> c == ' ')) 
    content <- many (satisfy (\c -> c /= '\n')) 
    rawContent <- getInput
    return Line {indent =  Prelude.length prefix, prefix = pack prefix, position = position, lineNumber = lineNumber, content = pack content}

slice :: Int -> Int -> [a] -> [a]
slice from to xs = Prelude.take (to - from + 1) (Prelude.drop from xs)


parseLine position lineNumber str = parseTest (lineParser position lineNumber) (pack str)