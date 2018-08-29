{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lox.Parser where

import           Data.Text                                ( Text
                                                          , pack
                                                          )
import           Data.Functor                             ( ($>) )
import           Data.Foldable

import           Control.Monad                 as M
import           Control.Monad.State                      ( State )
import qualified Control.Monad.State           as State
import qualified Control.Monad.Loops           as Loops
import           Control.Monad.Except          as E

import           Lox.Token
import           Lox.Expr
import           Lox.Utils                                ( ifM
                                                          , headMay
                                                          , tailSafe
                                                          )
import           Lox.Error


type ParserState = [Token]

newtype Parser a = Parser { runParser :: ExceptT LoxError (State ParserState) a}
    deriving (Functor, Applicative, Monad, MonadError LoxError, State.MonadState ParserState)

parse :: [Token] -> Either LoxError Expr
parse tokens = State.evalState (runExceptT $ runParser expression)
                               (filter (not . isIgnored) tokens)

-- expression → equality ;
expression :: Parser Expr
expression = equality

-- equality → comparison ( ( "!=" | "==" ) comparison )* ;
equality :: Parser Expr
equality = seqBinaryExpr comparison [BANG_EQUAL, EQUAL_EQUAL]

-- comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
comparison :: Parser Expr
comparison = seqBinaryExpr addition [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

-- addition → multiplication ( ( "-" | "+" ) multiplication )* ;
addition :: Parser Expr
addition = seqBinaryExpr multiplication [MINUS, PLUS]

-- multiplication → unary ( ( "/" | "*" ) unary )* ;
multiplication :: Parser Expr
multiplication = seqBinaryExpr unary [SLASH, STAR]

-- unary → ( "!" | "-" ) unary
--       | primary ;
unary :: Parser Expr
unary = match [BANG, MINUS] >>= \case
    Nothing       -> primary
    Just operator -> Unary operator <$> unary

-- primary → NUMBER | STRING | "false" | "true" | "nil"
--         | "(" expression ")" ;
primary :: Parser Expr
primary = advance >>= \t -> case t of
    Nothing -> throwError $ parseError Nothing "Unexpected end of token stream"
    Just token -> case tType token of
        (KEYWORD FALSE) -> pure $ Literal LitFalse
        (KEYWORD TRUE ) -> pure $ Literal LitTrue
        (KEYWORD NIL  ) -> pure $ Literal LitNil
        (STRING  s    ) -> pure $ Literal (LitString s)
        (NUMBER  n    ) -> pure $ Literal (LitNumber n)
        LEFT_PAREN      -> do
            e <- expression
            consume RIGHT_PAREN "Expect ')' after expression."
            pure $ Grouping e
        _ -> throwError $ parseError t "Expect expression"

-- |Parse an expression which is a sequence of other expression (with higher precedence),
-- separated by a list of given token type
seqBinaryExpr :: Parser Expr -> [TokenType] -> Parser Expr
seqBinaryExpr subExpr tokenTypes = do
    e  <- subExpr
    es <- Loops.whileJust (match [BANG_EQUAL, EQUAL_EQUAL])
        $ \operator -> fmap (operator, ) subExpr
    pure $ foldl' (\e1 (op, e2) -> Binary e1 op e2) e es


-- |If the next token's type matches one of the given type, consume and returns it
match :: [TokenType] -> Parser (Maybe Token)
match tokenTypes = do
    nextToken <- peek
    case nextToken of
        Nothing -> pure Nothing
        Just t  -> if tType t `elem` tokenTypes
            then advance $> Just t
            else pure Nothing

advance :: Parser (Maybe Token)
advance = do
    ts <- State.get
    State.modify tailSafe
    pure $ headMay ts

peek :: Parser (Maybe Token)
peek = State.gets headMay

consume :: TokenType -> Text -> Parser Token
consume expectedType errorMessage = do
    nextToken <- peek
    case nextToken of
        Just t | tType t == expectedType -> pure t
        _ -> throwError $ parseError nextToken errorMessage

parseError :: Maybe Token -> Text -> LoxError
parseError Nothing _ = LoxError (-1) Nothing "Unexpected end of token stream"
parseError (Just token) msg = LoxError
    (tLine token)
    (Just $ pack $ pos <> show (tType token))
    msg
    where pos = if tType token == EOF then "at end" else "at "

isIgnored :: Token -> Bool
isIgnored t = case tType t of
    (IGNORED _) -> True
    _           -> False
