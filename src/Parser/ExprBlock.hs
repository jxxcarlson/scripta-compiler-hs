{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Parser.ExprBlock (ExprBlock(..),BlockType(..), toExpressionBlock, Parser.ExprBlock.displayBlocks) where


import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List
import qualified Data.Map as Map
import Prelude hiding(init)
import Flow ((|>))
import Debug.Trace

import Parser.PrimitiveBlock
import Parser.Line (PrimitiveBlockType(..)) 
import Parser.Language (Language(..))  
import L0.Parser  
import Parser.Expr (Expr(..)) 
import qualified Parser.Expr

data ExprBlock
    = ExprBlock
        { name :: Maybe Text
        , args :: [Text]
        , properties :: Map Text Text
        , indent :: Int
        , lineNumber :: Int
        , numberOfLines :: Int
        , id :: Text
        , tag :: Text
        , blockType :: BlockType
        , content :: Either Text [Expr]
        , messages :: [Text]
        , sourceText :: Text
        } deriving (Show)

data BlockType
    = Paragraph
    | OrdinaryBlock [Text]
    | VerbatimBlock [Text]
    deriving(Show)


toExpressionBlock :: PrimitiveBlock -> ExprBlock
toExpressionBlock pb =
    let
        blockType_ =
            toBlockType (Parser.PrimitiveBlock.blockType pb) (drop 1 (Parser.PrimitiveBlock.args pb))

        linesOfText =  Parser.PrimitiveBlock.content pb 
        content_ = case blockType_ of 
             Paragraph ->  Right (linesOfText |> Text.unlines |> L0.Parser.run (Parser.PrimitiveBlock.lineNumber pb))
             OrdinaryBlock _ -> Right (linesOfText |> Text.unlines |> L0.Parser.run (Parser.PrimitiveBlock.lineNumber pb))
             VerbatimBlock _ -> Left (linesOfText |> Text.unlines)

    in
    ExprBlock
        { name = Parser.PrimitiveBlock.name pb
        , args = Parser.PrimitiveBlock.args pb
        , properties = Map.empty
        , indent = Parser.PrimitiveBlock.indent pb
        , lineNumber = Parser.PrimitiveBlock.lineNumber pb
        , numberOfLines = length linesOfText
        , id = Text.pack (show (Parser.PrimitiveBlock.lineNumber pb))
        , tag = "" -- Compiler.Util.getItem lang "label" sourceText
        , blockType = blockType_
        , content = content_
        , messages = [] -- MicroLaTeX.Parser.Expression.parseToState lineNumber sourceText |> MicroLaTeX.Parser.Expression.extractMessages
        , sourceText = Parser.PrimitiveBlock.sourceText pb
        } 


toBlockType :: PrimitiveBlockType -> [Text] -> BlockType
toBlockType pbt args =
    case pbt of
        PBParagraph ->
            Paragraph

        PBOrdinary ->
            OrdinaryBlock args

        PBVerbatim ->
            VerbatimBlock args


-- DISPLAY EXPRBLOCK

displayName :: ExprBlock -> Text
displayName block = 
    case Parser.ExprBlock.name block of 
        Nothing -> "name: anon"
        Just txt -> ["name:",  txt] |> Text.unwords

displayLineNumber :: ExprBlock -> Text
displayLineNumber block = 
    ["lineNumber:", (Text.pack . show) (Parser.ExprBlock.lineNumber block)] |> Text.unwords

displayIndentation :: ExprBlock -> Text
displayIndentation block = 
    ["indent:", (Text.pack . show) (Parser.ExprBlock.indent block)] |> Text.unwords


displayDict :: ExprBlock -> Text 
displayDict block = 
    -- ["dict:", (dict block) |> Map.toList  |> map yazzle  |> Text.unwords] |> Text.unwords
    ["properties:", (properties block) |> Map.toList |> map yazzle  |> Text.intercalate ", "] |> Text.unwords

yazzle :: (Text, Text)  -> Text
yazzle (a, b) =
    a <> ":" <> b


displayBlock :: ExprBlock -> Text
displayBlock block = 
    Text.unlines $ displayBlockType block 
      : displayLineNumber block 
      : displayIndentation block 
      : displayName block 
      : displayArgs block 
      : displayDict block 
      :  "------" 
      : [displayContent $ block] 

displayContent :: ExprBlock -> Text 
displayContent block = 
    case Parser.ExprBlock.content block of 
        Left txt -> txt
        Right exprs -> exprs |> map Parser.Expr.displayExpr |> Text.unwords

displayBlockType :: ExprBlock -> Text
displayBlockType block = 
    case Parser.ExprBlock.blockType block of 
        VerbatimBlock _ -> "type: Verbatim"
        OrdinaryBlock _ -> "type: Ordinary"
        Paragraph -> "type: Paragraph"

displayArgs :: ExprBlock -> Text
displayArgs block = 
    ("args:" : Parser.ExprBlock.args block) |> Text.intercalate ", "   


displayBlocks :: [ExprBlock] -> Text
displayBlocks blocks_ = 
   (map displayBlock blocks_) |> Text.unlines

