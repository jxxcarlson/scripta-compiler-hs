{-# LANGUAGE OverloadedStrings #-}

module Line(PrimitiveBlockType(..), Line, classify) where

import Data.Text.Lazy ( unpack, Text )
import Data.Text.Lazy.Encoding 

data PrimitiveBlockType = PBVerbatim | PBOrdinary | PBParagraph deriving (Show)

data Line =
    Line { indentL :: Int, prefixL :: Text, contentL :: Text, lineNumberL :: Int, positionL :: Int }


classify :: Int -> Int -> String -> Line
classify position lineNumber str =
    Line {indentL = 0, prefixL = "", contentL = "", lineNumberL = 0, positionL = 0}