module Parser.Expr (Expr(..)) where

import Parser.Meta (Meta)

data Expr
    = Fun String [Expr] Meta
    | Text String Meta
    | Verbatim String String Meta
