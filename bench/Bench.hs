{-# LANGUAGE OverloadedStrings #-}
      
import Criterion.Main 

import Data.Text (Text)
import qualified Data.Text as Text
import Parser.Language(Language(..))
import Parser.PrimitiveBlock (PrimitiveBlock)
import qualified Parser.PrimitiveBlock 
import Flow ((|>))
import qualified Scripta

main :: IO ()
main = defaultMain [
        bench "PrimitiveBlock.parse 1" $ whnf p text1
      , bench "PrimitiveBlock.parse 2" $ whnf p text2
      , bench "Scripta.compilie 1" $ whnf Scripta.compile text1
      , bench "Scripta.compilie 2" $ whnf Scripta.compile text2
   ]


p :: Text -> [PrimitiveBlock]
p txt = Parser.PrimitiveBlock.parse L0Lang (\_ -> False) (Text.lines txt)

primitiveBlocksText :: Text
primitiveBlocksText = "| section 1 foo:bar yada:a b c\nIntroduction\n\n\n|| image 71  46 width:300 caption:Primitive Steam Engine\nhttps://techmuseum.org/steam-engine.png\n\n\n"

paragraph1 = "| section 1 foo:bar yada:a b c\nIntroduction"
paragraph2 = repeat "This [i [b is] a test]" |> take 100 |> Text.unlines
paragraph3 = "|| image 71  46 width:300 caption:Primitive Steam Engine\nhttps://techmuseum.org/steam-engine.png"

text1 = [paragraph1, paragraph2, paragraph3] |> Text.intercalate "\n\n"

text2 = repeat text1 |> take 40 |> Text.intercalate "\n\n"