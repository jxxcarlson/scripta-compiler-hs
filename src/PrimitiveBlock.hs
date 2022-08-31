{-# LANGUAGE OverloadedStrings #-}


module PrimitiveBlock (PrimitiveBlock, empty, Step(..), loop) where

import Data.Text.Lazy ( unpack, Text )
import Data.Text.Lazy.Encoding 
import Line (PrimitiveBlockType,Line,classify)   

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
    , linesS :: [Text]
    , inBlock :: Bool
    , indentS :: Int
    , lineNumberS :: Int
    , positionS :: Int
    , inVerbatim :: Bool
    , isVerbatimLine :: Text -> Bool
    , count :: Int
    , label :: Text
    }

data Language = L0Lang | MicroLaTeXLang deriving  (Show)

data Step state a
    = Loop state
    | Done a



{-| Parse a list of strings into a list of primitive blocks given a markup
language and a function for determining when a string is the first line
of a verbatim block
-}
parse ::Language -> (Text -> Bool) ->  [Text] ->  [PrimitiveBlock]
parse lang isVerbatimLine lines =
    case lang of
        L0Lang ->
            lines |> parse_ lang isVerbatimLine

        MicroLaTeXLang ->
            --lines |> MicroLaTeX.Parser.TransformLaTeX.toL0 |> parse_ lang isVerbatimLine
            lines |> parse_ lang isVerbatimLine

        
parse_ :: Language -> (String -> Bool) ->  [Text] -> [PrimitiveBlock]
parse_ lang isVerbatimLine lines =
    loop (init lang isVerbatimLine lines) nextStep
        |> map (\block -> finalize block)

head_ :: [a] -> Maybe a 
head_ [] = Nothing 
head_ (first:srest) = Just first

nextStep :: State -> Step State  [PrimitiveBlock]
nextStep state =
    case head_ $ linesS $ state of
        Nothing ->
            case currentBlock state of
                Nothing ->
                    Done (reverse $ blocks $ state)

                Just block ->
                    let
                        blocks =
                            if content block == [ "" ] then
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                reverse (blocks state)

                            else
                                -- Debug.log (Tools.cyan "****, DONE" 13)
                                reverse (block :: blocks state)
                    in
                    Done blocks

        Just rawLine ->
            let
                newPosition =
                    if rawLine == "" then
                        position state + 1

                    else
                        position state + length rawLine + 1

                currentLine :: Line
                currentLine =
                    -- TODO: the below is wrong
                    Line.classify (position state) (lineNumber state + 1) rawLine
            in
            case ( inBlock state, isEmpty currentLine, isNonEmptyBlank currentLine ) of
                -- not in a block, pass over empty line
                ( False, True, _ ) ->
                    Loop (advance newPosition state{label = "1, EMPTY" })

                -- not in a block, pass over blank, non-empty line
                ( False, False, True ) ->
                    Loop (advance newPosition state{label = "2, PASS" })

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


