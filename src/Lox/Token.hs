module Lox.Token where

import           Data.Text                      ( Text )

data Keyword = AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR |
  PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE deriving (Show, Eq)

data TokenType =
    -- Single-character tokens.
    LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |
    COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR |

    -- One or two character tokens.
    BANG | BANG_EQUAL |
    EQUAL | EQUAL_EQUAL |
    GREATER | GREATER_EQUAL |
    LESS | LESS_EQUAL |

    -- Literals.
    IDENTIFIER Text | STRING Text | NUMBER Double | COMMENT Text |

    -- -- Keywords.
    -- AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR |
    -- PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE |
    KEYWORD Keyword |

    EOF | IGNORED Char

    deriving (Show, Eq)

data Token = Token
    { tType :: TokenType
    , tLine :: Int
    } deriving Show
