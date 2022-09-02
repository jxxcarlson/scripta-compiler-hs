{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, DuplicateRecordFields, OverloadedLabels #-}

-- OverloadedRecordDot
-- https://pure-hack.com/posts/overloaded-labels-in-haskell/
-- https://cpufun.substack.com/p/setting-up-the-apple-m1-for-native
-- https://www.reddit.com/r/haskell/comments/tqzxy1/now_that_stackage_supports_ghc_92_is_it_easy_to/

module PrimitiveBlock (PrimitiveBlock, empty, parse) where

import Data.Text.Lazy as Text (Text, concat, intercalate, length, fromChunks, strip)
import Line (PrimitiveBlockType(..),Line,indent, isEmpty, getNameAndArgs, prefix, content, lineNumber, position, classify, getBlockType) 
import Prelude
import Language (Language(..)) 
import Flow ((|>))
import Debug.Trace

data PrimitiveBlock = PrimitiveBlock
    { indent :: Int
    , lineNumber :: Int
    , position  :: Int
    , args :: [Text]
    , content  :: [Text]
    , name  :: Maybe Text
    , named :: Bool
    , blockType  :: PrimitiveBlockType
    , sourceText :: Text
    } deriving (Show, Eq)


empty :: PrimitiveBlock
empty = 
    PrimitiveBlock
    { indent = 0
    , lineNumber = 0
    , position = 0
    , content = []
    , name = Nothing
    , args = []
    , named = False
    , sourceText = ""
    , blockType = PBParagraph
    }


data State =
   State { blocks ::  [PrimitiveBlock]
    , currentBlock :: Maybe PrimitiveBlock
    , lang :: Language
    , lines_ :: [Text]
    , inBlock :: Bool
    , inVerbatim :: Bool
    , indent :: Int
    , currentLineNumber :: Int
    , cursor :: Int
    , isVerbatimLine :: Text -> Bool
    , count :: Int
    , label :: Text
    } 

xlog :: Show a => String -> a -> a
xlog msg a = Debug.Trace.trace (msg <> ": " <> show a) a

instance Show State where
  show state = 
    -- do your string formatting here, possibly calling `show` on fields
    show (((count state), fmap PrimitiveBlock.content (currentBlock state), map PrimitiveBlock.content $ blocks state) )

init :: Language -> (Text -> Bool) ->  [Text] -> State
init lang isVerbatimLine lines =
   State{ blocks = []
    , currentBlock = Nothing
    , lang = lang
    , lines_ = lines
    , indent = 0
    , currentLineNumber = 0
    , cursor = 0
    , inBlock = False
    , inVerbatim = False
    , isVerbatimLine = isVerbatimLine
    , count = 0
    , label = "0, START"
    } |> xlog "initial state "

data Step state a
    = Loop state
    | Done a



{-| Parse a list of strings into a list of primitive blocks given a markup
language and a function for determining when a string is the first line
of a verbatim block
-}
parse :: Language -> (Text -> Bool) ->  [Text] ->  [PrimitiveBlock]
parse lang isVerbatimLine lines_ =
    case lang of
        L0Lang ->
            lines_ |> xlog "parse, lines (1)" |> parse2 lang isVerbatimLine |> xlog "OUT"

        MicroLaTeXLang ->
            --lines_ |> MicroLaTeX.Parser.TransformLaTeX.toL0 |> parse_ lang isVerbatimLine
            lines_ |> parse2 lang isVerbatimLine

        
parse2 :: Language -> (Text -> Bool) ->  [Text] -> [PrimitiveBlock]
parse2 lang isVerbatimLine lines2 =
    loop (PrimitiveBlock.init lang isVerbatimLine lines2) nextStep
        |> map (\block -> finalize block)


head_ :: [Text] -> Maybe Text
head_ [] = Nothing |> xlog "head_ (1)"
head_ (first:srest) = Just first |> xlog "head_ (2)"

nextStep :: State -> Step State  [PrimitiveBlock]
nextStep state =
    case head_ $ lines_ $ state of
        Nothing ->
            case currentBlock state of
                Nothing ->
                    Done (reverse $ blocks $ (state |> xlog "DONE, state"))

                Just block ->
                    let
                        newBlocks =
                            if PrimitiveBlock.content block == [ "" ] then
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                reverse (blocks state)

                            else
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                reverse (block : (blocks state))
                    in
                    Done (newBlocks |> xlog "Done, newBlocks")

        Just rawLine ->
            let
                newCursor =
                    (if rawLine == "" then
                        cursor state + 1

                    else
                        cursor state + (Text.length rawLine |> fromIntegral) + 1) |> xlog "rawLine, cursor"

                currentLine :: Line
                currentLine =
                    -- TODO: the below is wrong
                    Line.classify (cursor state) (currentLineNumber state + 1) rawLine
            in
            case ( inBlock state, Line.isEmpty currentLine, isNonEmptyBlank currentLine ) of
                -- not in a block, pass over empty line
                ( False, True, _ ) ->
                    Loop (advance newCursor state{label = "1, EMPTY" }) 

                -- not in a block, pass over blank, non-empty line
                ( False, False, True ) ->
                    Loop (advance newCursor state{label = "2, PASS" })

                -- create a new block: we are not in a block, but
                -- the current line is nonempty and nonblank
                ( False, False, False ) ->
                    Loop (createBlock state{label = "3, NEW" } currentLine)

                -- A nonempty line was encountered inside a block, so add it
                ( True, False, _ ) ->
                    Loop (addCurrentLine2 state{label = "4, ADD" } currentLine)

                -- commit the current block: we are in a block and the
                -- current line is empty
                ( True, True, _ ) ->
                    Loop (commitBlock state{label = "5, COMMIT" } currentLine)


loop :: state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ -> loop s_ f
        Done b -> b


-- FUNCTIONS

finalize :: PrimitiveBlock -> PrimitiveBlock
finalize block =
    let
        finalContent =
           reverse (PrimitiveBlock.content block)

        sourceText =
            -- String.join "\n" content
            Text.intercalate "\n" finalContent

    in
    block{content = finalContent, sourceText = sourceText }


isNonEmptyBlank :: Line -> Bool
isNonEmptyBlank line =
   Line.indent line > 0 && Line.content line == ""


advance :: Int -> State -> State
advance newCursor state =
    state{lines_ = drop 1 (lines_ state)
        , currentLineNumber = (currentLineNumber state) + 1
        , cursor = newCursor
        , count = (count state) + 1
    } |> xlog "advance"

createBlock :: State -> Line -> State
createBlock state currentLine =
    let
        newBlocks =
            case currentBlock state of
                Nothing ->
                    blocks state

                -- When creating a new block push the current block onto state.blocks
                -- only if its content is nontrivial (not == [""])
                Just block ->
                    if PrimitiveBlock.content block == [] then
                        blocks state

                    else
                        block : (blocks state)

        newBlock =
            Just (blockFromLine (lang state) currentLine)
    in
    state{lines_ = drop 1 (lines_ state)
        , currentLineNumber = (currentLineNumber state) + 1
        , cursor = (cursor state) + (Text.length (Line.content currentLine) |> fromIntegral)
        , count = (count state) + 1
        , indent = (Line.indent currentLine)
        , inBlock = True
        , currentBlock = newBlock
        , blocks = newBlocks
    } |> xlog "createBlock"

blockFromLine :: Language -> Line -> PrimitiveBlock
-- blockFromLine lang ({ indent, lineNumber, position, prefix, content } as line) =
blockFromLine lang line =
   PrimitiveBlock { indent = Line.indent line
    , lineNumber = Line.lineNumber line
    , position = Line.position line
    , content =  [""] -- [Text.fromChunks [Line.prefix line, Line.content line]]
    , name = Nothing
    , args = []
    , named = False
    , sourceText = ""
    , blockType = Line.getBlockType lang (Line.content line)
    }
        |> elaborate line


elaborate :: Line -> PrimitiveBlock -> PrimitiveBlock
elaborate line pb =
    if named pb then
        pb

    else 
        if (PrimitiveBlock.content pb) == [ "" ] then
        pb

        else
            let
                ( name, args ) =
                    -- TODO: note this change: it needs to be verified
                    Line.getNameAndArgs line

                content =
                    if (blockType pb) == PBVerbatim then

                        map Text.strip (PrimitiveBlock.content pb)

                    else (PrimitiveBlock.content pb)
            in
            pb{ content = content, name = name, args = args, named = True } |> xlog "elaborate"


addCurrentLine2 :: State -> Line -> State
addCurrentLine2 state currentLine =
    case currentBlock state of
        Nothing ->
            state{ lines_ = Prelude.drop 1 (lines_ state) } |> xlog "addCurrentLine2 (1)"

        Just block ->
            state{lines_ = Prelude.drop 1 (lines_ state)
                , currentLineNumber = currentLineNumber state + 1
                , cursor = (cursor state)+ (Text.length (Line.content currentLine) |> fromIntegral)
                , count = (count state) + 1
                , currentBlock =
                    Just (addCurrentLine currentLine block)
            } |> xlog "addCurrentLine2 (2)"

addCurrentLine :: Line -> PrimitiveBlock -> PrimitiveBlock
addCurrentLine line block =
    let
        pb =
            addCurrentLine_ line block |> xlog "addCurrentLine"
    in
    elaborate line pb            


addCurrentLine_ :: Line -> PrimitiveBlock -> PrimitiveBlock
addCurrentLine_ line block =
    if blockType block == PBVerbatim then
        if name block == Just "math" then
            block{  content = Line.content line : PrimitiveBlock.content block 
            ,sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]} |> xlog "addCurrentLine_ (1)"

        else
            block{ content = (Text.concat [Line.prefix line,  Line.content line]) : PrimitiveBlock.content block
            ,sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}  |> xlog "addCurrentLine_ (2)"

    else
        block{ content = Line.content line :  PrimitiveBlock.content block
         ,sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}  |> xlog "addCurrentLine_ (3)"
 


commitBlock :: State -> Line -> State
commitBlock state currentLine =
    case currentBlock state of
        Nothing ->
            state{ 
                 lines_ = Prelude.drop 1 (lines_ state)
                , indent = Line.indent currentLine
            } |> xlog "commitBlock (1)"

        Just block ->
            let
                ( currentBlock, newBlocks ) =
                    if PrimitiveBlock.content block == [ "" ] then
                        ( Nothing, blocks state )

                    else
                        ( Just (blockFromLine (lang state) currentLine), block : blocks state )
            in
            state{ 
                 lines_ = Prelude.drop 1 (lines_ state)
                , currentLineNumber = currentLineNumber state + 1
                , cursor = cursor state + (Text.length (Line.content currentLine) |> fromIntegral)
                , count = count state + 1
                , blocks = newBlocks
                , inBlock = False
                , inVerbatim = (isVerbatimLine state) (Line.content currentLine)
                , currentBlock = currentBlock
            } |> xlog "commitBlock (2)"
