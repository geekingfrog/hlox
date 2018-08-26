{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad as M
import Data.Semigroup ((<>))
import Data.Functor (($>))

import qualified System.Environment as Env
import qualified System.Exit as Exit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8')

import qualified Lox.Token
import qualified Lox.Scanner as Scanner
import qualified Lox.Error as Err

main :: IO ()
main = Env.getArgs >>= \case
    [file] -> runFile file
    [] -> runPrompt
    _ -> putStrLn "Usage: jlox [script]" *> Exit.exitWith (Exit.ExitFailure 64)


runFile filePath = do
    content <- BS.readFile filePath
    case decodeUtf8' content of
        Left  err     -> print err *> Exit.exitWith (Exit.ExitFailure 64)
        Right decoded -> run decoded >>= \case
            Success -> pure ()
            Failure err ->
                Err.reportLoxError err *> Exit.exitWith (Exit.ExitFailure 65)


runPrompt = M.forever $ do
    putStr "> "
    l <- BS.getLine
    case decodeUtf8' l of
      Left err -> print err *> Exit.exitWith (Exit.ExitFailure 64)
      Right decoded -> run decoded $> ()

data LoxResult = Success | Failure Err.LoxError

run :: Text -> IO LoxResult
run source = do
    mapM_ print (Scanner.scanTokens source)
    pure Success
