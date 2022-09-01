{-# LANGUAGE OverloadedStrings #-}


module XParser where

-- https://serokell.io/blog/parser-combinators-in-haskell

import Control.Applicative (Alternative (..))
import Data.List (nub)

data Error i e
  = EndOfInput  -- Expected more input, but there is nothing
  | Unexpected i  -- We didn't expect to find this element
  | CustomError e  -- Extra errors the user may want to create
  | Empty  -- Used in `Alternative` implementation of `empty`
  deriving (Eq, Show)


newtype Parser i e a = Parser
  { runParser :: [i] -> Either [Error i e] (a, [i])
  }

satisfy :: (i -> Bool) -> Parser i e i
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left [EndOfInput]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left [Unexpected hd]

char :: Eq i => i -> Parser i e i
char i = satisfy (== i)

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) -> Right (f output, rest)



{- Or this:

instance Functor (Parser i e) where
  fmap f (Parser p) = Parser $ \input -> do
    (output, rest) <- p input
    pure (f output, rest)
    
-}

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input ->
    case f input of
      Left err -> Left err
      Right (f', rest) ->
        case p rest of
          Left err -> Left err
          Right (output, rest') -> Right (f' output, rest')

{-| Or this:

instance Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)

  Parser f <*> Parser p = Parser $ \input -> do
    (f', rest) <- f input
    (output, rest') <- p rest
    pure (f' output, rest')
    
-}

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input ->
    case p input of
      Left err -> Left err
      Right (output, rest) ->
        let
          Parser p' = k output
        in
        p' rest


{-| Or this:

instance Monad (Parser i e) where
  return = pure

  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest

-}

string :: Eq i => [i] -> Parser i e [i]
string = traverse char

{-| Where

If traverse is new to you, it’s also equivalent to these definitions:

string :: Eq i => [i] -> Parser i e [i]
string [] = pure []
string (x : xs) = (:) <$> char x <*> string xs
Or even in terms of Monad:

string [] = return []
string (x : xs) = do
  y <- char x
  ys <- string xs
  return (y : ys)

-}

instance (Eq i, Eq e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left [Empty]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)