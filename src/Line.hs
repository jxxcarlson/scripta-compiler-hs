{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Line(PrimitiveBlockType(..), Line, indent, prefix, content, lineNumber, position, classify, getBlockType) where

import Data.Text.Lazy as Text ( unpack, Text, take, strip )
import Data.Text.Lazy.Encoding 
import Language (Language(..)) 

data PrimitiveBlockType = PBVerbatim | PBOrdinary | PBParagraph deriving (Show)

data Line =
    Line { indent :: Int, prefix :: Text, content :: Text, lineNumber :: Int, position :: Int }


classify :: Int -> Int -> String -> Line
classify position lineNumber str =
    Line {indent = 0, prefix = "", content = "", lineNumber = 0, position = 0}

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