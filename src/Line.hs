{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Line(PrimitiveBlockType(..), Line, getNameAndArgs, isEmpty, indent, prefix,content, lineNumber, position, classify, getBlockType) where

-- https://markkarpov.com/tutorial/megaparsec.html

import Data.Text.Lazy as Text ( unpack, Text, take, strip, words, drop )
import Data.Functor.Identity (Identity)
import Data.Maybe(maybe)
import Text.Megaparsec

import Language (Language(..)) 


type Parsec e s a = ParsecT e s Identity a

data PrimitiveBlockType = PBVerbatim | PBOrdinary | PBParagraph deriving (Show, Eq)

data Line =
    Line { indent :: Int, prefix :: Text, content :: Text, lineNumber :: Int, position :: Int }


classify :: Int -> Int -> Text -> Line
classify position lineNumber str =
    Line {indent = 0, prefix = "", content = "", lineNumber = 0, position = 0}

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