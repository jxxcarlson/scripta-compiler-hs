{-# LANGUAGE OverloadedStrings #-}

module TestData (foo, p) where

import Data.Text.Lazy as Text (Text, pack, lines)
import PrimitiveBlock (PrimitiveBlock, parse)
import Language (Language(..))
import Flow ((|>))

foo = "abc\n\ndef"

p :: String -> [PrimitiveBlock]
p str = parse L0Lang (\_ -> False) (str |> Text.pack |> Text.lines)