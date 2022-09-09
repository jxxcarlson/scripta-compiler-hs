{-# LANGUAGE OverloadedStrings #-}

module Render.Block (render, renderSpan) where 

    -- view-source:https://sixthform.info/katex/guide.html

import Control.Monad (forM_)
import Text.Blaze.Html5 as H

import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

import Parser.ExprBlock (ExprBlock(..), BlockType(..))
import Parser.Expr(Expr(..))

render1 :: [ExprBlock] -> String
render1 blocks =  renderHtml $ H.div $ toHtml $ Prelude.map render_ blocks 


render :: [ExprBlock] -> String
render blocks = 
 do
   renderHtml $ toHtml $ Prelude.map render_ blocks 



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
        Right exprs -> p $ toHtml $ Prelude.map renderExpr exprs

renderExpr :: Expr -> Html
renderExpr expr = 
    case expr of 
        Text txt _ -> toHtml txt
        Fun name body _ -> 
            case Map.lookup name functionDict of
                Nothing ->  H.span $ toHtml $ "Element " <> name <> " not yet implemented"
                Just f -> f body
               
        Verbatim name body _ -> 
            case name of 
                "math" -> H.span $ toHtml $ "\\(" <> body <> "\\)"



functionDict :: Map Text ([Expr] -> Html)
functionDict = Map.fromList [
       ("i", \body -> em (toHtml $ Prelude.map renderExpr body))
    ,  ("b", \body -> strong (toHtml $ Prelude.map renderExpr body)) 
    , ("red", \body ->renderSpan "color:red" body )    
    , ("blue", \body ->renderSpan "color:blue" body )    
    , ("highlight", \body ->renderSpan "background-color:yellow" body )    
    , ("bluelight", \body ->renderSpan "background-color:#A7C7E7" body )    
   ]



--renderSpan :: AttributeValue -> [Expr] -> Html
renderSpan style_ exprs = 
    H.span ! (A.style style_)  $ (toHtml $ Prelude.map renderExpr exprs)

renderOrdinaryBlock :: [Text] -> ExprBlock -> Html
renderOrdinaryBlock args block  =   p "Ordinary block: rendering not implemented"  

renderVerbatimBlock :: [Text] -> ExprBlock -> Html
renderVerbatimBlock args block  =   p "Verbatim block: rendering not implemented"  