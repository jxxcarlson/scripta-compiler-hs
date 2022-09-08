{-# LANGUAGE OverloadedStrings #-}

module Render.Block (render) where 

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Data.Text (Text)
import qualified Data.Text as Text

import Parser.ExprBlock (ExprBlock(..), BlockType(..))
import Parser.Expr(Expr(..))

render :: [ExprBlock] -> String
render blocks = 
   renderHtml $ H.div $ toHtml $ Prelude.map render_ blocks 

render_ :: ExprBlock -> Html
render_ block = 
    case Parser.ExprBlock.blockType block of 
        Paragraph -> renderContent (Parser.ExprBlock.content block)
        OrdinaryBlock args -> renderOrdinaryBlock args block
        VerbatimBlock args -> renderVerbatimBlock args block

renderContent :: Either Text [Expr] -> Html
renderContent input = 
    case input of 
        Left txt -> p $ toHtml txt 
        Right exprs -> p "Right exprs: rendering not implemented"

renderOrdinaryBlock :: [Text] -> ExprBlock -> Html
renderOrdinaryBlock args block  =   p "Ordinary block: rendering not implemented"  

renderVerbatimBlock :: [Text] -> ExprBlock -> Html
renderVerbatimBlock args block  =   p "Verbatim block: rendering not implemented"  