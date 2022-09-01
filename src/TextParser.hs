
module TextParser (test, lineParser, textParser) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Flow ((|>))
import Data.Text.Lazy as Text ( pack, Text, take, strip, words, drop )
import System.Environment

-- type Parser s = ParsecT Data.Void.Void s Identity Text

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

lineParser_ :: Parser String
lineParser_ = 
  many (satisfy (\c -> c /= '\n'))

eolP :: Parser Char
eolP = satisfy (\c -> c == '\n') 


lineParser2 :: Parser Char
lineParser2 = lineParser >>= (\s -> eolP)

first :: Parser a -> Parser b -> Parser a
first pa pb = do
    a <- pa
    b <- pb
    return a

lineParser = first lineParser_ eolP

textParser = many lineParser


-- *TextParser> test "abc\ndef\nghi\n"
-- ["abc","def","ghi"]
test str = parseTest textParser str