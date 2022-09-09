{-# LANGUAGE OverloadedStrings #-}

module Render.Block (render, renderSpan) where 

    -- view-source:https://sixthform.info/katex/guide.html
    -- https://gdevanla.github.io/posts/read-you-a-blaze.html
    -- https://mmhaskell.com/blog/2020/3/9/blaze-lightweight-html-generation

import Control.Monad (forM_)
import Text.Blaze.Html5 as H

import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map
import Flow ((|>))
import qualified Log (xlog)

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
        OrdinaryBlock args -> renderOrdinaryBlock block
        VerbatimBlock args -> renderVerbatimBlock block

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

renderOrdinaryBlock :: ExprBlock -> Html
renderOrdinaryBlock block  =  
    case (Parser.ExprBlock.name block) of 
        Nothing -> p "Ordinary block: error (no name)" 
        Just "section" ->  
            case (head_ (Parser.ExprBlock.args block) |> Log.xlog "!! ARGS" |> fmap Text.strip) of 
                Nothing -> h1 $ renderContent (Parser.ExprBlock.content block) 
                Just "1" -> h1 $ renderContent (Parser.ExprBlock.content block) 
                Just "2" -> h2 $ renderContent (Parser.ExprBlock.content block) 
                Just "3" -> h3 $ renderContent (Parser.ExprBlock.content block) 
                Just "4" -> h4 $ renderContent (Parser.ExprBlock.content block)
                Just _ ->   h5 $ renderContent (Parser.ExprBlock.content block)    
           

        Just name ->  p $ toHtml $ "Error: ordinary block for " <> name <> " not implemented"
         

renderVerbatimBlock :: ExprBlock -> Html
renderVerbatimBlock block  = 
    case (Parser.ExprBlock.name block) of 
        Nothing -> p "Error: a verbatim block cannot be anonymous" 
        Just "equation" -> p $ toHtml $ "\\["  <> verbatimContent block <> "\\]"
        Just "math" -> p $ toHtml $ "\\["  <> verbatimContent block <> "\\]"
        Just "image" -> renderImage block
        Just name ->  p $ toHtml $ "Error: verbatim block for " <> name <> " not implemented"

birdUrl :: AttributeValue
birdUrl = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQCE2i4ctUkAD6x8p9EK2QyQobseDGta40fHg&usqp=CAU"

renderImage block = 
    let 
        w = case Map.lookup "width" (properties block) of 
            Nothing -> "400"
            Just w -> w

        claas = case Map.lookup "position" (properties block) of 
            Just "center" -> "center"
            _ -> "foo"

        caption = case Map.lookup "caption" (properties block) of 
            Nothing -> ""
            Just caption_ -> caption_

        url = textValue $ verbatimContent block

    in
    H.div $ do
        img ! A.src url ! A.width (textValue w) ! A.class_ claas
        p ! A.class_ claas $ toHtml caption
           
verbatimContent :: ExprBlock -> Text
verbatimContent block = 
    case (Parser.ExprBlock.content block) of 
        Left txt -> txt
        Right _ -> ""

head_ :: [a] -> Maybe a 
head_ [] = Nothing
head_ (first:rest)  = Just first