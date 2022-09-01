
module MyParser (singleLetterP, ptest) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Data.Text.Lazy as Text ( unpack, Text, take, strip, words, drop )
import System.Environment

-- type Parser s = ParsecT Data.Void.Void s Identity Text

type Parser = Parsec Data.Void.Void String


singleLetterP :: Parser Char
singleLetterP = char 'h'

bool :: Parser Bool
bool = False <$ string "false" <|> True <$ string "true"



ptest str = parseTest bool str

-- lineParser :: Int -> Int -> Parser Line
-- lineParser position lineNumber =
--     Parser.succeed (\prefixStart prefixEnd lineEnd content
--     -> {   indent = prefixEnd - prefixStart
--          , prefix = String.slice 0 prefixEnd content
--          , content = String.slice prefixEnd lineEnd content
--          , position = position
--          , lineNumber = lineNumber }
--        )
--         |= Parser.getOffset
--         |. Parser.chompWhile (\c -> c == ' ')
--         |= Parser.getOffset
--         |. Parser.chompWhile (\c -> c /= '\n')
--         |= Parser.getOffset
--         |= Parser.getSource
