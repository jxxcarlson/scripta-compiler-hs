{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, OverloadedLabels #-}

-- OverloadedRecordDot
-- https://pure-hack.com/posts/overloaded-labels-in-haskell/

module PrimitiveBlock (PrimitiveBlock, empty, Step(..), loop) where

import Data.Text.Lazy as Text (Text, intercalate, empty, length, fromChunks, strip)
import Line (PrimitiveBlockType(..),Line,indent, prefix, content, lineNumber, position, classify, getBlockType) 
import Language (Language(..)) 
import Flow ((|>))

data PrimitiveBlock = PrimitiveBlock
    { indent :: Int
    , lineNumber :: Int
    , position  :: Int
    , args :: [Text]
    , content  :: [Text]
    , name  :: Text
    , named :: Bool
    , blockType  :: PrimitiveBlockType
    , sourceText :: Text
    } deriving (Show)


empty :: PrimitiveBlock
empty = 
    PrimitiveBlock
    { indent = 0
    , lineNumber = 0
    , position = 0
    , content = []
    , name = "anon"
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
    , indent :: Int
    , currentLineNumber :: Int
    , cursor :: Int
    , inVerbatim :: Bool
    , isVerbatimLine :: Text -> Bool
    , count :: Int
    , label :: Text
    }


data Step state a
    = Loop state
    | Done a



{-| Parse a list of strings into a list of primitive blocks given a markup
language and a function for determining when a string is the first line
of a verbatim block
-}
parse ::Language -> (Text -> Bool) ->  [Text] ->  [PrimitiveBlock]
parse lang isVerbatimLine lines_ =
    case lang of
        L0Lang ->
            lines_ |> parse_ lang isVerbatimLine

        MicroLaTeXLang ->
            --lines_ |> MicroLaTeX.Parser.TransformLaTeX.toL0 |> parse_ lang isVerbatimLine
            lines_ |> parse_ lang isVerbatimLine

        
parse_ :: Language -> (String -> Bool) ->  [Text] -> [PrimitiveBlock]
parse_ lang isVerbatimLine lines_ =
    loop (init lang isVerbatimLine lines_) nextStep
        |> map (\block -> finalize block)

head_ :: [a] -> Maybe a 
head_ [] = Nothing 
head_ (first:srest) = Just first

nextStep :: State -> Step State  [PrimitiveBlock]
nextStep state =
    case head_ $ lines_ $ state of
        Nothing ->
            case currentBlock state of
                Nothing ->
                    Done (reverse $ blocks $ state)

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
                    Done newBlocks

        Just rawLine ->
            let
                newCursor =
                    if rawLine == "" then
                        cursor state + 1

                    else
                        cursor state + (Text.length rawLine |> fromIntegral) + 1

                currentLine :: Line
                currentLine =
                    -- TODO: the below is wrong
                    Line.classify (cursor state) (currentLineNumber state + 1) rawLine
            in
            case ( inBlock state, currentLine == Text.empty, isNonEmptyBlank currentLine ) of
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
    PrimitiveBlock{content = finalContent, sourceText = sourceText }


isNonEmptyBlank :: Line -> Bool
isNonEmptyBlank line =
   Line.indent line > 0 && Line.content line == Text.empty


advance :: Int -> State -> State
advance newCursor state =
    state{lines_ = drop 1 (lines_ state)
        , currentLineNumber = (currentLineNumber state) + 1
        , cursor = newCursor
        , count = (count state) + 1
    }

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
    }

blockFromLine :: Language -> Line -> PrimitiveBlock
-- blockFromLine lang ({ indent, lineNumber, position, prefix, content } as line) =
blockFromLine lang line =
   PrimitiveBlock { indent = Line.indent line
    , lineNumber = Line.lineNumber line
    , position = Line.position line
    , content = Text.fromChunks [Line.prefix, Line.content] 
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
        if (content pb) == [ "" ] then
        pb

        else
            let
                ( name, args ) =
                    -- TODO: note this change: it needs to be verified
                    Line.getNameAndArgs L0Lang line

                content =
                    if (blockType pb) == PBVerbatim then
                        map Text.strip (content pb)

                    else
                        (content pb)
            in
            pb{ content = content, name = name, args = args, named = True }
