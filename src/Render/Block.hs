{-# LANGUAGE OverloadedStrings #-}

module Render.Block (render) where 

    -- view-source:https://sixthform.info/katex/guide.html

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Data.Text (Text)
import qualified Data.Text as Text

import Parser.ExprBlock (ExprBlock(..), BlockType(..))
import Parser.Expr(Expr(..))

render1 :: [ExprBlock] -> String
render1 blocks =  renderHtml $ H.div $ toHtml $ Prelude.map render_ blocks 


render :: [ExprBlock] -> String
render blocks = 
 do
        --  H.title "Scripta-hs Demo"
--         let link1 = H.link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css" -- ! A.integrity "sha384-bYdxxUwYipFNohQlHt0bjN/LCpueqWz13HufFEV1SUatKs1cm4L6fFgCi1jT643X" ! A.crossorigin "anonymous"
--         let script1 = H.script ! A.src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" -- ! A.integrity "sha384-Qsn9KnoKISj6dI8g7p1HBlNpVx0I8p1SvlwOldgi3IorMle61nQy4zEahWYtlja" -- ! A.crossorigin"anonymous"
--         let script2 = H.script ! A.src "https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js" ! A.onload "renderMathInElement(document.body);"
-- --         onload="renderMathInElement(document.body);"
--         -- ! integrity "sha384-bYdxxUwYipFNohQlHt0bjN/LCpueqWz13HufFEV1SUatKs1cm4L6fFgCi1jT643X" 
--         -- renderHtml $ toHtml $ [docType, link1, script1, script2, toHtml $ Prelude.map render_ blocks]
--         let hdStuff = toHtml [docType, "<script src=\"aaa\"></script>", link1, script1, script2]
        renderHtml $ toHtml $ Prelude.map render_ blocks 


-- <head>
--     <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.css" integrity="sha384-bYdxxUwYipFNohQlHt0bjN/LCpueqWz13HufFEV1SUatKs1cm4L6fFgCi1jT643X" crossorigin="anonymous">

--     <!-- The loading of KaTeX is deferred to speed up page rendering -->
--     <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/katex.min.js" integrity="sha384-Qsn9KnoKISj6dI8g7p1HBlNpVx0I8p1SvlwOldgi3IorMle61nQy4zEahWYtljaz" crossorigin="anonymous"></script>

--     <!-- To automatically render math in text elements, include the auto-render extension: -->
--     <script defer src="https://cdn.jsdelivr.net/npm/katex@0.16.2/dist/contrib/auto-render.min.js" integrity="sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05" crossorigin="anonymous"
--         onload="renderMathInElement(document.body);"></script>
-- </head>

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
            case name of 
                "i" -> em (toHtml $ Prelude.map renderExpr body)
                "b" -> strong (toHtml $ Prelude.map renderExpr body)
                _ -> H.span $ toHtml $ "Element " <> name <> " not yet implemented"
        Verbatim name body _ -> 
            case name of 
                "math" -> H.span $ toHtml $ "\\(" <> body <> "\\)"


renderOrdinaryBlock :: [Text] -> ExprBlock -> Html
renderOrdinaryBlock args block  =   p "Ordinary block: rendering not implemented"  

renderVerbatimBlock :: [Text] -> ExprBlock -> Html
renderVerbatimBlock args block  =   p "Verbatim block: rendering not implemented"  