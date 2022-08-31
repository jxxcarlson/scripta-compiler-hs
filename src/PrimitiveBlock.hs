
module PrimitiveBlock (PrimitiveBlock, empty) where

data PrimitiveBlock = PrimitiveBlock
    { indent :: Int
    , lineNumber :: Int
    , position  :: Int
    , content  :: [String]
    , name  :: String
    , args  :: [String]
    , named  :: Bool
    , sourceText  :: String
    , blockType  :: PrimitiveBlockType
    } deriving (Show)

data PrimitiveBlockType = PBVerbatim | PBOrdinary | PBParagraph deriving (Show)

empty :: PrimitiveBlock
empty = 
    PrimitiveBlock
    { indent = 0
    , lineNumber = 0
    , position = 0
    , content = []
    , name = "anon"
    , args = []
    , named = False
    , sourceText = ""
    , blockType = PBParagraph
    }
