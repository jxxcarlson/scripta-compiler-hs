module Compiler.Scripta (compile) where

import Data.Text (Text)
import qualified Data.Text as Text   
import Flow ((|>)) 

import  Compiler.Parser.PrimitiveBlock (PrimitiveBlock(..))
import qualified Compiler.Parser.PrimitiveBlock as Parser.PrimitiveBlock
import qualified Compiler.L0.Parser as L0.Parser
import Compiler.Parser.ExprBlock (ExprBlock(..))
import qualified Compiler.Parser.ExprBlock as Parser.ExprBlock
import Compiler.Parser.Language (Language(..))

compile :: Text -> [ExprBlock]
compile sourceText = 
    sourceText 
      |> Text.lines
      |> Parser.PrimitiveBlock.parse L0Lang (\_ -> False) 
      |> map Parser.ExprBlock.toExpressionBlock
