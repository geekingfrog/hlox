{-# LANGUAGE OverloadedStrings #-}

module Lox.Scanner where

import           Control.Monad.State           as State
import qualified Control.Monad.Loops           as Loops
import           Data.Text                     as T
import           Data.Functor
import qualified Data.Char                     as C
import qualified Data.Map.Strict               as Map

import           Lox.Token
import           Lox.Error
import           Lox.Utils                      ( ifM )

type ScanResult = Either LoxError Token

data ScannerState = ScannerState
    { src :: !Text -- remaining source to scan
    , line :: Int -- current line number (for error reporting)
    } deriving Show

scanTokens :: Text -> [ScanResult]
scanTokens source =
    State.evalState (Loops.unfoldM scanToken >>= addEOF) (ScannerState source 1)

scanToken :: State ScannerState (Maybe ScanResult)
scanToken = do
    source <- State.gets src
    mbC    <- advance
    case mbC of
        Nothing -> pure Nothing
        Just c  -> parseChar c

parseChar :: Char -> State ScannerState (Maybe ScanResult)
parseChar c = case c of
    '(' -> addSingleToken LEFT_PAREN
    ')' -> addSingleToken RIGHT_PAREN
    '{' -> addSingleToken LEFT_BRACE
    '}' -> addSingleToken RIGHT_BRACE
    ',' -> addSingleToken COMMA
    '.' -> addSingleToken DOT
    '-' -> addSingleToken MINUS
    '+' -> addSingleToken PLUS
    ';' -> addSingleToken SEMICOLON
    '*' -> addSingleToken STAR
    '/' -> ifM (match '/') parseComment (addSingleToken SLASH)
    '!' -> ifM (match '=') (addSingleToken BANG_EQUAL) (addSingleToken BANG)
    '=' -> ifM (match '=') (addSingleToken EQUAL_EQUAL) (addSingleToken EQUAL)
    '<' -> ifM (match '=') (addSingleToken LESS_EQUAL) (addSingleToken LESS)
    '>' ->
        ifM (match '=') (addSingleToken GREATER_EQUAL) (addSingleToken GREATER)

    -- ignore whitespaces
    ' '           -> addSingleToken (IGNORED ' ')
    '\r'          -> addSingleToken (IGNORED '\r')
    '\t'          -> addSingleToken (IGNORED '\t')
    '\n'          -> incLine *> addSingleToken (IGNORED '\n')

    '"'           -> parseString
    _ | isDigit c -> parseNumber c
    _ | isAlpha c -> parseIdentifier c
    _             -> unexpectedChar c

advance :: State ScannerState (Maybe Char)
advance = do
    st <- State.get
    case T.uncons (src st) of
        Nothing        -> pure Nothing
        Just (c, rest) -> do
            State.put $ st { src = rest }
            pure $ Just c

addSingleToken :: TokenType -> State ScannerState (Maybe ScanResult)
addSingleToken typ = do
    l <- State.gets line
    pure $ Just $ Right $ Token typ l

addEOF :: [ScanResult] -> State ScannerState [ScanResult]
addEOF tokens = do
    l <- State.gets line
    pure $ tokens <> [Right $ Token EOF l]

parseComment :: State ScannerState (Maybe ScanResult)
parseComment = do
    comment <- Loops.unfoldM $ do
        x <- peek
        case x of
            Nothing   -> pure Nothing
            Just '\n' -> pure Nothing
            Just _    -> advance
    addSingleToken (COMMENT $ T.pack comment)

parseString :: State ScannerState (Maybe ScanResult)
parseString = do
    str <- Loops.unfoldM $ do
        x <- peek
        case x of
            Nothing  -> pure $ Just Nothing
            Just '"' -> advance $> Nothing
            Just c   -> do
                when (c == '\n') incLine
                void advance
                pure $ Just (Just c)
    case sequence str of
        Nothing  -> unterminatedString
        Just str -> addSingleToken (STRING $ T.pack str)

parseNumber :: Char -> State ScannerState (Maybe ScanResult)
parseNumber firstDigit = do
    raw <- Loops.unfoldM $ do
        x <- peek
        case x of
            Nothing  -> pure Nothing
            Just '.' -> do
                next <- peekNext
                case next of
                    Nothing            -> pure Nothing
                    Just n | isDigit n -> advance $> x
                    _                  -> pure Nothing
            Just n | isDigit n -> advance $> Just n
            _                  -> pure Nothing
    case parseDouble (firstDigit : raw) of
        Nothing -> do
            l <- State.gets line
            pure $ Just $ Left $ LoxError
                l
                Nothing
                (T.pack $ "Invalid number: " <> raw)
        Just d -> addSingleToken (NUMBER d)

parseDouble :: String -> Maybe Double
parseDouble raw = case reads raw of
    [(d, "")] -> Just d
    _         -> Nothing

parseIdentifier :: Char -> State ScannerState (Maybe ScanResult)
parseIdentifier firstChar = do
    str <- Loops.unfoldM $ do
        x <- peek
        case x of
            Nothing                   -> pure Nothing
            Just c | isAlphaNumeric c -> advance $> Just c
            _                         -> pure Nothing
    let s = firstChar : str
    case Map.lookup s keywordsMap of
        Nothing -> addSingleToken (IDENTIFIER $ T.pack s)
        Just kw -> addSingleToken (KEYWORD kw)

keywordsMap :: Map.Map String Keyword
keywordsMap = Map.fromList
    [ ("and"   , AND)
    , ("class" , CLASS)
    , ("else"  , ELSE)
    , ("false" , FALSE)
    , ("for"   , FOR)
    , ("fun"   , FUN)
    , ("if"    , IF)
    , ("nil"   , NIL)
    , ("or"    , OR)
    , ("print" , PRINT)
    , ("return", RETURN)
    , ("super" , SUPER)
    , ("this"  , THIS)
    , ("true"  , TRUE)
    , ("var"   , VAR)
    , ("while" , WHILE)
    ]

unexpectedChar :: Char -> State ScannerState (Maybe ScanResult)
unexpectedChar c = do
    l <- State.gets line
    pure $ Just $ Left $ LoxError l Nothing (T.snoc "Unexpected character." c)

unterminatedString :: State ScannerState (Maybe ScanResult)
unterminatedString = do
    l <- State.gets line
    pure $ Just $ Left $ LoxError l Nothing "Unterminated string."

incLine = modify' (\s -> s { line = line s + 1 })

match :: Char -> State ScannerState Bool
match c = do
    source <- State.gets src
    if T.null source || T.head source /= c then pure False else advance $> True

peek :: State ScannerState (Maybe Char)
peek = do
    s <- State.gets src
    pure $ fmap fst (T.uncons s)

peekNext :: State ScannerState (Maybe Char)
peekNext = do
    s <- State.gets src
    pure $ fmap fst $ fmap snd (T.uncons s) >>= T.uncons

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = C.isAsciiLower c || c == '_'

isAlphaNumeric c = isAlpha c || isDigit c

test :: IO ()
test = do
    c <- readFile "test.lox"
    print $ scanTokens $ T.pack c
