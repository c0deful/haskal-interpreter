module Interpreter where

import Control.Monad

import Types

{- A monad for keeping memory store and passing errors. -}

newtype Interpreter a = Interpreter { runInterpreter :: Store -> Either String (a, Store) }

instance Monad Interpreter where
    return x = Interpreter $ \s -> Right (x, s)
    i >>= k  = Interpreter $ \s -> case runInterpreter i s of
               Left msg -> Left msg
               Right (x, s') -> runInterpreter (k x) s'
    fail msg = Interpreter $ \_ -> Left msg