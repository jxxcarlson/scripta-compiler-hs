module L0.Token (tokenParser_, pp) where
import qualified Data.Text as Text 
import Data.Text (Text) 
import Data.List
import Data.List.Index (imap)

import Text.Megaparsec (parseMaybe, choice, satisfy, many, takeWhileP, getOffset, Parsec, Token, MonadParsec)
import Text.Megaparsec.Char (string)
import Data.Void
import Flow ((|>))

-- TYPES

type TokenParser = Parsec Data.Void.Void Text


data L0Token
    = LB Meta
    | RB Meta
    | S Text Meta
    | W Text Meta
    | MathToken Meta
    | BracketedMath Text Meta
    | CodeToken Meta
    deriving (Show)
    -- | TokenError (List (DeadEnd Context Problem)) Meta

data Meta = Meta { begin :: Int, end :: Int, index :: Int} deriving(Show)


incrementIndex :: Int -> L0Token -> L0Token
incrementIndex k token =
  changeMeta (incrementMeta k) token

incrementMeta :: Int -> Meta -> Meta
incrementMeta k meta = meta{index = k + (index meta)}
  
changeMeta :: (Meta -> Meta) -> L0Token -> L0Token
changeMeta changeMeta token =
    case token of 
        LB meta -> LB (changeMeta meta)
        RB meta -> RB (changeMeta meta)
        S txt meta -> S txt (changeMeta meta)
        W txt meta -> W txt (changeMeta meta)
        MathToken meta -> MathToken (changeMeta meta)
        BracketedMath txt meta ->  BracketedMath txt (changeMeta meta)
        CodeToken meta -> CodeToken (changeMeta meta)

extractMeta :: (Meta -> a) -> L0Token -> a
extractMeta extract token =
    case token of 
        LB meta -> extract meta
        RB meta -> extract meta
        S txt meta -> extract meta
        W txt meta -> extract meta
        MathToken meta -> extract meta
        BracketedMath txt meta ->  extract meta
        CodeToken meta -> extract meta


setIndex :: Int -> L0Token -> L0Token
setIndex k token = 
    changeMeta (\meta -> meta{index = k}) token



data State a =
    State { source :: String
    , scanpointer :: Int
    , tokenIndex :: Int
    , sourceLength :: Int
    , tokens :: [a]
    , currentToken :: Maybe a
    , mode :: Mode
    }


data Mode
    = Normal
    | InMath
    | InCode


data TokenType
    = TLB
    | TRB
    | TS
    | TW
    | TMath
    | TBracketedMath
    | TCode
    | TTokenError


type_ :: L0Token -> TokenType
type_ token =
    case token of 
        LB _ -> TLB
        RB _ -> TRB
        S txt _ -> TS
        W txt _ -> TW
        MathToken _ -> TBracketedMath
        BracketedMath _ _ ->  TBracketedMath
        CodeToken _ -> TCode


getMeta :: L0Token -> Meta
getMeta token =
    case token of 
        LB meta -> meta
        RB meta -> meta
        S _ meta -> meta
        W _ meta -> meta
        MathToken meta -> meta
        BracketedMath _ meta ->  meta
        CodeToken meta -> meta


toString :: [L0Token] -> String
toString tokens =
   map show tokens |> mconcat


length :: L0Token -> Int
length token = extractMeta (\meta -> (end meta) - (begin meta)) token


-- init : String -> State a
-- init str =
--     { source = str
--     , scanpointer = 0
--     , sourceLength = String.length str
--     , tokens = []
--     , currentToken = Nothing
--     , tokenIndex = 0
--     , mode = Normal
--     }


-- type alias TokenParser =
--     Parser Context Problem Token


-- run : String -> List Token
-- run source =
--     loop (init source) nextStep


-- {-|

--     NOTES. In the computation of the end field of the Meta component of a Token,
--     one must use the code `end = start + data.end - data.begin  - 1`.  The
--     `-1` is because the data.end comes from the position of the scanPointer,
--     which is at this juncture pointing one character beyond the string chomped.

-- -}
-- get : State Token -> Int -> String -> Token
-- get state start input =
--     case Parser.run (tokenParser state.mode start state.tokenIndex) input of
--         Ok token ->
--             token

--         Err errorList ->
--             TokenError errorList { begin = start, end = start + 1, index = state.tokenIndex }


-- nextStep : State Token -> Step (State Token) (List Token)
-- nextStep state =
--     if state.scanpointer >= state.sourceLength then
--         case state.currentToken of
--             Just token ->
--                 Done (token :: state.tokens)

--             Nothing ->
--                 Done state.tokens

--     else
--         let
--             token =
--                 get state state.scanpointer (String.dropLeft state.scanpointer state.source)

--             newScanPointer =
--                 state.scanpointer + length token + 1

--             ( tokens, tokenIndex, currentToken_ ) =
--                 if isTextToken token then
--                     -- if head of the token list is a left bracket token, commit the text token immediately:
--                     -- it is the expected function name
--                     if Maybe.map type_ (List.head state.tokens) == Just TLB then
--                         ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

--                     else
--                         -- otherwise, update the current token so as to merge words into a single phrase
--                         ( state.tokens, state.tokenIndex, updateCurrentToken state.tokenIndex token state.currentToken )

--                 else if type_ token == TLB then
--                     -- commit a left bracket token immediately, taking care to commit the currentToken if it contains text
--                     case state.currentToken of
--                         Nothing ->
--                             ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

--                         Just textToken ->
--                             ( setIndex (state.tokenIndex + 1) token :: setIndex state.tokenIndex textToken :: state.tokens, state.tokenIndex + 2, Nothing )

--                 else
--                     -- the token is neither a left bracket token nore a text token.  Commit it immediatley, taking care
--                     -- to also commit the currentToken if it holds text.
--                     case state.currentToken of
--                         Nothing ->
--                             ( setIndex state.tokenIndex token :: state.tokens, state.tokenIndex + 1, Nothing )

--                         Just textToken ->
--                             ( setIndex (state.tokenIndex + 1) token :: textToken :: state.tokens, state.tokenIndex + 2, Nothing )

--             currentToken =
--                 if isTextToken token then
--                     currentToken_

--                 else
--                     Nothing
--         in
--         Loop
--             { state
--                 | tokens = tokens
--                 , scanpointer = newScanPointer
--                 , tokenIndex = tokenIndex
--                 , currentToken = currentToken
--                 , mode = newMode token state.mode
--             }


-- updateCurrentToken : Int -> Token -> Maybe Token -> Maybe Token
-- updateCurrentToken index token currentToken =
--     case currentToken of
--         Nothing ->
--             Just (setIndex index token)

--         Just token_ ->
--             Just <| setIndex index (mergeToken token_ token)


-- isTextToken : Token -> Bool
-- isTextToken token =
--     List.member (type_ token) [ TW, TS ]


-- mergeToken : Token -> Token -> Token
-- mergeToken lastToken currentToken =
--     let
--         lastTokenMeta =
--             getMeta lastToken

--         currentTokenMeta =
--             getMeta currentToken

--         meta =
--             { begin = lastTokenMeta.begin, end = currentTokenMeta.end, index = -1 }
--     in
--     S (stringValue lastToken ++ stringValue currentToken) meta


-- newMode : Token -> Mode -> Mode
-- newMode token currentMode =
--     case currentMode of
--         Normal ->
--             case token of
--                 MathToken _ ->
--                     InMath

--                 CodeToken _ ->
--                     InCode

--                 _ ->
--                     Normal

--         InMath ->
--             case token of
--                 MathToken _ ->
--                     Normal

--                 _ ->
--                     InMath

--         InCode ->
--             case token of
--                 CodeToken _ ->
--                     Normal

--                 _ ->
--                     InCode


{-| Expression.Tokenizer.tokenParser calls L1.tokenParser
with arguments tokenStack and start. The first argument
is not used (although it is for the Markdown parser)
-}
-- tokenParser : Mode -> Int -> Int -> TokenParser
-- tokenParser mode start index =
--     case mode of
--         Normal ->
--             tokenParser_ start index

--         InMath ->
--             mathParser_ start index

--         InCode ->
--             codeParser_ start index


languageChars =
    [ '[', ']', '`', '$', '\\' ]


mathChars =
    [ '$' ]


codeChars =
    [ '`' ]


pp start index txt = case (parseMaybe (many (tokenParser_ start index)) txt) of 
            Nothing -> Nothing
            Just tokens -> Just $ imap incrementIndex  tokens





tokenParser_ :: Int -> Int -> TokenParser L0Token
tokenParser_ start index =
    choice
        [ textParser start index
        , leftBracketParser start index
        , rightBracketParser start index
        -- , bracketedMathParser start index
        , mathParser start index
        , codeParser start index
        , whiteSpaceParser start index
        ]


-- mathParser_ : Int -> Int -> TokenParser
-- mathParser_ start index =
--     Parser.oneOf
--         [ mathTextParser start index
--         , mathParser start index
--         , whiteSpaceParser start index
--         ]


-- codeParser_ : Int -> Int -> TokenParser
-- codeParser_ start index =
--     Parser.oneOf
--         [ codeTextParser start index
--         , codeParser start index
--         , whiteSpaceParser start index
--         ]


-- whiteSpaceParser : Int -> Int -> TokenParser
-- whiteSpaceParser start index =
--     PT.text (\c -> c == ' ') (\c -> c == ' ')
--         |> Parser.map (\data -> W data.content { begin = start, end = start, index = index })



whiteSpaceParser :: Int -> Int -> TokenParser L0Token
whiteSpaceParser start index = 
    do
      first <- satisfy (\c -> c == ' ')
      rest <- many (satisfy (\c -> c == ' '))
      return $ W (Text.pack (first : rest)) (Meta { begin = start, end = start, index = index })

rightBracketParser :: Int -> Int -> TokenParser L0Token
rightBracketParser start index = 
    do
      a <- getOffset
      satisfy (\c -> c == ']')
      return $ RB (Meta { begin = start + a, end = start + a, index = index })


leftBracketParser :: Int -> Int -> TokenParser L0Token
leftBracketParser start index = 
    do
      satisfy (\c -> c == '[')
      return $ LB (Meta { begin = start, end = start, index = index })



-- bracketedMathParser : Int -> Int -> TokenParser
-- bracketedMathParser start index =
--     Parser.succeed (\a b content -> BracketedMath (String.slice a (b - 2) content) { begin = start, end = start + b - a + 1, index = index })
--         |. Parser.symbol (Parser.Token "\\[" (PT.ExpectingSymbol "\\["))
--         |= Parser.getOffset
--         |. Parser.chompUntil (Parser.Token "\\]" (PT.ExpectingSymbol "\\]"))
--         |. Parser.symbol (Parser.Token "\\]" (PT.ExpectingSymbol "\\]"))
--         |= Parser.getOffset
--         |= Parser.getSource


-- bracketedMathParser :: Int -> Int -> TokenParser L0Token
-- bracketedMathParser start index = 
--     do
--         a <- getOffset
--         first <- string "\\[" :: TokenParser Text
--         rest <- takeWhileP Nothing (/='\\') :: TokenParser Text
--         b <- getOffset
--         satisfy (=='\\')
--         satisfy (==']')
--         return (BracketedMath (rest) (Meta { begin = start, end = start + b - a + 1, index = index }) )
        

textParser :: Int -> Int -> TokenParser L0Token
textParser start index = 
    do
      a <- getOffset
      first <- satisfy (\c -> not $ Data.List.elem c (' ' : languageChars))
      rest <- ( many $ satisfy (\c -> not $ Data.List.elem c (' ' : languageChars)))
      b <- getOffset
      let content = Text.pack (first : rest)
      return $ S content (Meta { begin = start + a, end = start + b - 1, index = index })



mathTextParser :: Int -> Int -> TokenParser L0Token
mathTextParser start index = 
    do
      satisfy (\c -> not $ Data.List.elem c (' ' : mathChars))
      content_ <- ( many $ satisfy (\c -> not $ Data.List.elem c (' ' : languageChars)))
      let content = Text.pack content_
      return $ S content (Meta { begin = start, end = start + (Text.length content) - 1, index = index })


codeTextParser :: Int -> Int -> TokenParser L0Token
codeTextParser start index = 
    do
      satisfy (\c -> not $ Data.List.elem c (' ' : codeChars))
      content_ <- ( many $ satisfy (\c -> not $ Data.List.elem c (' ' : languageChars)))
      let content = Text.pack content_
      return $ S content (Meta { begin = start, end = start + (Text.length content) - 1, index = index })


mathParser :: Int -> Int -> TokenParser L0Token
mathParser start index = 
    do
      satisfy (\c -> c == '$')
      return $ MathToken (Meta { begin = start, end = start, index = index })


codeParser :: Int -> Int -> TokenParser L0Token
codeParser start index = 
    do
      satisfy (\c -> c == '`')
      return $ CodeToken (Meta { begin = start, end = start, index = index })


-- lineParser :: Int -> Int -> LineParser Line
-- lineParser position_ lineNumber_ = 
--   do 
--     prefix_ <- many (satisfy (\c -> c == ' ')) 
--     content_ <- many (satisfy (\c -> c /= '\n')) 
--     return Line {indent =  Prelude.length prefix_, prefix = Text.pack prefix_, position = position_, lineNumber = lineNumber_, content = Text.pack content_}

