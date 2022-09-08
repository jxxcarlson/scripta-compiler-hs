module Parser.Expr (Expr(..)) where

import Parser.Meta (Meta)
import qualified Data.Text

data Expr
    = Fun Data.Text.Text [Expr] Meta
    | Text Data.Text.Text Meta
    | Verbatim String Data.Text.Text Meta
    deriving(Show)
