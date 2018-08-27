{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Lox.Parser where

import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Foldable

import           Control.Monad                 as M
import           Control.Monad.State            ( State )
import qualified Control.Monad.State           as State
import qualified Control.Monad.Loops           as Loops

import           Lox.Token
import           Lox.Expr
import           Lox.Utils                      ( ifM
                                                , headMay
                                                , tailSafe
                                                )
import           Lox.Error

type ParserState = [Token]

parse :: [Token] -> Either LoxError Expr
parse tokens = State.evalState expression (filter (not . isIgnored) tokens)

-- expression → equality ;
expression :: State ParserState (Either LoxError Expr)
expression = equality

-- equality → comparison ( ( "!=" | "==" ) comparison )* ;
equality :: State ParserState (Either LoxError Expr)
equality = seqBinaryExpr comparison [BANG_EQUAL, EQUAL_EQUAL]

-- comparison → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
comparison :: State ParserState (Either LoxError Expr)
comparison = seqBinaryExpr addition [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]

-- addition → multiplication ( ( "-" | "+" ) multiplication )* ;
addition :: State ParserState (Either LoxError Expr)
addition = seqBinaryExpr multiplication [MINUS, PLUS]

-- multiplication → unary ( ( "/" | "*" ) unary )* ;
multiplication :: State ParserState (Either LoxError Expr)
multiplication = seqBinaryExpr unary [SLASH, STAR]

-- unary → ( "!" | "-" ) unary
--       | primary ;
unary :: State ParserState (Either LoxError Expr)
unary = match [BANG, MINUS] >>= \case
    Nothing       -> primary
    Just operator -> (fmap . fmap) (Unary operator) unary

-- primary → NUMBER | STRING | "false" | "true" | "nil"
--         | "(" expression ")" ;
primary :: State ParserState (Either LoxError Expr)
primary = advance >>= \t -> case t of
    Nothing ->
        pure $ Left $ parseError Nothing "Unexpected end of token stream"
    Just token -> case tType token of
        (KEYWORD FALSE) -> pure $ Right $ Literal LitFalse
        (KEYWORD TRUE ) -> pure $ Right $ Literal LitTrue
        (KEYWORD NIL  ) -> pure $ Right $ Literal LitNil
        (STRING  s    ) -> pure $ Right $ Literal (LitString s)
        (NUMBER  n    ) -> pure $ Right $ Literal (LitNumber n)
        LEFT_PAREN      -> do
            e <- expression
            consume RIGHT_PAREN "Expect ')' after expression." >>= \case
                Nothing  -> pure $ fmap Grouping e
                Just err -> pure $ Left err
        _ -> pure $ Left $ parseError t "Expect expression"

-- |Parse an expression which is a sequence of other expression (with higher precedence),
-- separated by a list of given token type
seqBinaryExpr
    :: State ParserState (Either LoxError Expr)
    -> [TokenType]
    -> State ParserState (Either LoxError Expr)
seqBinaryExpr subExpr tokenTypes = do
    e  <- subExpr
    es <- Loops.whileJust (match [BANG_EQUAL, EQUAL_EQUAL])
        $ \operator -> (fmap . fmap) (operator, ) subExpr

    case (e, sequence es) of
        (Left err, _       ) -> pure $ Left err
        (_       , Left err) -> pure $ Left err
        (Right e, Right es) ->
            pure $ Right $ foldl' (\e1 (op, e2) -> Binary e1 op e2) e es

-- |If the next token's type matches one of the given type, consume and returns it
match :: [TokenType] -> State ParserState (Maybe Token)
match tokenTypes = do
    nextToken <- peek
    case nextToken of
        Nothing -> pure Nothing
        Just t  -> if tType t `elem` tokenTypes
            then advance $> Just t
            else pure Nothing

advance :: State ParserState (Maybe Token)
advance = do
    ts <- State.get
    State.modify tailSafe
    pure $ headMay ts

peek :: State ParserState (Maybe Token)
peek = State.gets headMay

consume :: TokenType -> Text -> State ParserState (Maybe LoxError)
consume expectedType errorMessage = do
    nextToken <- peek
    case nextToken of
        Just t | tType t == expectedType -> pure Nothing
        _ -> pure $ Just $ parseError nextToken errorMessage

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
