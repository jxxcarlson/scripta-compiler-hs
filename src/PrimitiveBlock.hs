{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, DuplicateRecordFields, OverloadedLabels #-}

-- OverloadedRecordDot
-- https://pure-hack.com/posts/overloaded-labels-in-haskell/
-- https://cpufun.substack.com/p/setting-up-the-apple-m1-for-native
-- https://www.reddit.com/r/haskell/comments/tqzxy1/now_that_stackage_supports_ghc_92_is_it_easy_to/

module PrimitiveBlock (PrimitiveBlock, content, empty, parse, displayBlocks) where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Line
import Line (PrimitiveBlockType(..),Line) 
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
    } deriving (Eq)


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
    show (((label state), map PrimitiveBlock.content $ blocks state) )

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
    }

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
            lines_ |> parse_ lang isVerbatimLine 

        MicroLaTeXLang ->
            --lines_ |> MicroLaTeX.Parser.TransformLaTeX.toL0 |> parse_ lang isVerbatimLine
            lines_ |> parse_ lang isVerbatimLine

        
parse_ :: Language -> (Text -> Bool) ->  [Text] -> [PrimitiveBlock]
parse_ lang isVerbatimLine lines2 =
    loop (PrimitiveBlock.init lang isVerbatimLine lines2) nextStep
        |> map (\block -> finalize block)


head_ :: [Text] -> Maybe Text
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
                    (if rawLine == "" then
                        cursor state + 1 

                    else
                        cursor state + (Text.length rawLine |> fromIntegral) + 1) 

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
                    Loop (addCurrentLine state{label = "4, ADD" } currentLine)

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
blockFromLine lang line =
   PrimitiveBlock { indent = Line.indent line
    , lineNumber = Line.lineNumber line
    , position = Line.position line
    , content =  [Line.content line] -- [Text.fromChunks [Line.prefix line, Line.content line]]
    , name = Nothing
    , args = []
    , named = False
    , sourceText = ""
    , blockType = Line.getBlockType lang (Line.content line)
    }
        |> elaborate line 


-- elaborate1 :: Line -> PrimitiveBlock -> PrimitiveBlock
-- elaborate1 line pb =
--     if named pb then
--         pb

--     else 
--         if (PrimitiveBlock.content pb) == [ "" ] then
--         pb

--         else
--             let
--                 ( name, args ) =
--                     -- TODO: note this change: it needs to be verified
--                     Line.getNameAndArgs line

--                 content =
--                     if (blockType pb) == PBVerbatim then

--                         map Text.strip (PrimitiveBlock.content pb)

--                     else (PrimitiveBlock.content pb)
--             in
--             pb{ content = content, name = name, args = args, named = True }


elaborate :: Line -> PrimitiveBlock -> PrimitiveBlock
elaborate line pb =
    let
        ( name, args ) =
            Line.getNameAndArgs line

        content = 
            case blockType pb  of
                PBParagraph -> PrimitiveBlock.content pb
                PBOrdinary -> PrimitiveBlock.content pb |> drop 1
                PBVerbatim -> PrimitiveBlock.content pb |> drop 1 |> map Text.strip

    in
    pb{ content = content, name = name, args = args, named = True }


addCurrentLine :: State -> Line -> State
addCurrentLine state currentLine =
    case currentBlock state of
        Nothing ->
            state{ lines_ = Prelude.drop 1 (lines_ state) } 

        Just block ->
            state{lines_ = Prelude.drop 1 (lines_ state)
                , currentLineNumber = currentLineNumber state + 1
                , cursor = (cursor state)+ (Text.length (Line.content currentLine) |> fromIntegral)
                , count = (count state) + 1
                , currentBlock =
                    Just (addCurrentLine_ currentLine block)
            }          


addCurrentLine_ :: Line -> PrimitiveBlock -> PrimitiveBlock
addCurrentLine_ line block =
    if blockType block == PBVerbatim then
        if name block == Just "math" then
            block{  content = Line.content line : PrimitiveBlock.content block 
            ,sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}

        else
            block{ content = (Text.concat [Line.prefix line,  Line.content line]) : PrimitiveBlock.content block
            ,sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}  

    else
        block{ content = Line.content line :  PrimitiveBlock.content block
         ,sourceText = Text.concat [sourceText block, "\n",  Line.prefix line, Line.content line ]}  
 


commitBlock :: State -> Line -> State
commitBlock state currentLine =
    case currentBlock state of
        Nothing ->
            state{ 
                 lines_ = Prelude.drop 1 (lines_ state)
                , indent = Line.indent currentLine
            } 

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
            }


-- DISPLAY PRIMITIVEBLOXK

displayName :: PrimitiveBlock -> Text
displayName block = 
    case name block of 
        Nothing -> "name: anon"
        Just txt -> ["name: ",  txt] |> Text.unwords

displayBlock :: PrimitiveBlock -> Text
displayBlock block = 
    Text.unlines $ displayBlockType block : displayName block : displayArgs block : "------" : (PrimitiveBlock.content $ block  ) 

displayBlockType :: PrimitiveBlock -> Text
displayBlockType block = 
    case blockType block of 
        PBVerbatim -> "type: Verbatim"
        PBOrdinary -> "type: Ordinary"
        PBParagraph -> "type: Paragraph"

displayArgs :: PrimitiveBlock -> Text
displayArgs block = 
    ("args: " : args block) |> Text.unwords   


displayBlocks :: [PrimitiveBlock] -> Text
displayBlocks blocks_ = 
   (map displayBlock blocks_) |> Text.unlines

