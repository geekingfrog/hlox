module Lox.Error where

import Data.Text as T

data LoxError = LoxError
    { leLine :: Int
    , leWhere :: Maybe Text
    , leMessage :: Text
    } deriving Show

reportLoxError err =
    putStrLn
        $  "[line "
        <> show (leLine err)
        <> "] Error"
        <> maybe "" T.unpack (leWhere err)
        <> ": "
        <> T.unpack (leMessage err)
