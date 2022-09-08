module Scripta (compile) where

import Data.Text (Text)
import qualified Data.Text as Text   
import Flow ((|>)) 

import  Parser.PrimitiveBlock (PrimitiveBlock(..))
import qualified Parser.PrimitiveBlock 
import qualified L0.Parser
import Parser.ExprBlock (ExprBlock(..))
import qualified Parser.ExprBlock
import Parser.Language (Language(..))

compile :: Text -> [ExprBlock]
compile sourceText = 
    sourceText 
      |> Text.lines
      |> Parser.PrimitiveBlock.parse L0Lang (\_ -> False) 
      |> map Parser.ExprBlock.toExpressionBlock
