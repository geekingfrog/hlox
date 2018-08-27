module Lox.Expr where

import           Data.Text                      ( Text )
import           Lox.Token

data Stmt

type Operator = Token

data LiteralValue
    = LitTrue
    | LitFalse
    | LitNil
    | LitString !Text
    | LitNumber !Double
    deriving Show

data Expr
    = Binary Expr Operator Expr
    | Unary Operator Expr
    | Literal LiteralValue
    | Grouping Expr
    deriving Show
