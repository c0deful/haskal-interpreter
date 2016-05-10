module Main where

import System.Environment
import System.IO

import ParHaskal
import LexHaskal
import AbsHaskal
import ErrM

import Interpreter
import Runner
import Utils

{- Unfortunately, output will be printed only if no runtime errors occur. -}

main = do
    args <- getArgs
    code <- if null args
        then getContents
        else readFile $ head args
    case pBlock (myLexer code) of
        Ok b    -> case runInterpreter (run b emptyEnv) emptyStore of
                       Left msg        -> hPutStrLn stderr ("Runtime error: " ++ msg)
                       Right (outs, _) -> mapM_ putStrLn outs
        Bad err -> hPutStrLn stderr ("Parser error: " ++ err)
