{-# LANGUAGE OverloadedStrings #-}

module TestData (p, xstr)where



import Data.Text.Lazy as Text (Text, pack, lines)
import PrimitiveBlock (PrimitiveBlock, parse)
import Language (Language(..))
import Flow ((|>))
import TextParser()

xstr = "abc\ndef\n\nuvw\n\n"

p :: String -> [PrimitiveBlock]
p str = parse L0Lang (\_ -> True) (str |> Text.pack |> Text.lines)