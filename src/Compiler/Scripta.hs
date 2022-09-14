module Compiler.Scripta (compile, compleToHtmlString, compleToHtmlText) where

import Prelude

import Data.Text
import Data.Text.Internal.Lazy    
import Flow ((|>)) 

import Compiler.Parser.PrimitiveBlock (PrimitiveBlock(..))
import qualified Compiler.Parser.PrimitiveBlock as Parser.PrimitiveBlock
import qualified Compiler.L0.Parser as L0.Parser
import Compiler.Parser.ExprBlock (ExprBlock(..))
import qualified Compiler.Parser.ExprBlock as Parser.ExprBlock
import Compiler.Parser.Language (Language(..))
import qualified Compiler.Render.Block

compile :: Data.Text.Text -> [ExprBlock]
compile sourceText = 
    sourceText 
      |> Data.Text.lines
      |> Parser.PrimitiveBlock.parse L0Lang (\_ -> False) 
      |> Prelude.map Parser.ExprBlock.toExpressionBlock


compleToHtmlString :: Data.Text.Text -> String
compleToHtmlString sourceText = 
  sourceText |> compile |> Compiler.Render.Block.renderToString


compleToHtmlText :: Data.Text.Text -> Data.Text.Internal.Lazy.Text
compleToHtmlText sourceText = 
  sourceText |> compile |> Compiler.Render.Block.renderToText 