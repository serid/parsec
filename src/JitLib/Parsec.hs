{-# LANGUAGE TupleSections #-}

module JitLib.Parsec where

import Control.Applicative

import JitLib.Result
import JitLib.Util

data ParsingError = Undef
                  | UnexpectedChar
                  | Eof
                  | ExpectedLexeme String
                  | TrailingCharacters String
                  deriving Show

data Parser a = Parser { runParser :: String -> Result ParsingError (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input ->
        do (input', a) <- p input
           Ok (input', f a)


instance Applicative Parser where
    pure a = Parser $ Ok . (,a)
    (<*>) = liftA2 id where
            liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
            liftA2 f (Parser p1) (Parser p2) = Parser $ \input ->
                do (input', a) <- p1 input
                   (input'', b) <- p2 input'
                   Ok (input'', f a b)

instance Monad Parser where
    (Parser p) >>= k = Parser $ \input ->
        do (input', a) <- p input
           let (Parser p2) = k a
           p2 input'

instance Alternative Parser where
    empty = Parser $ const (Err Undef)
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        p1 input <||> p2 input

-- Complement function to Parser.pure
dirty :: ParsingError -> Parser a
dirty e = Parser $ const (Err e)

-- Parses zero or more of "p"
manyP :: Parser a -> Parser [a]
manyP p = do a <- p
             as <- manyP p
             pure (a:as)
          <|> pure []

-- Tries to apply the parser in it's argument, but does not fail if the parser fails
maybeParseOrMaybeDon't :: Parser () -> Parser ()
maybeParseOrMaybeDon't p = p <|> pure ()

-- Char parser
charP :: Parser Char
charP = Parser f where
    f (c:input') = Ok (input', c)
    f [] = Err Eof

-- Char matcher
matchCharP :: Char -> Parser ()
matchCharP c1 = do c2 <- charP
                   if c1 == c2
                   then pure ()
                   else dirty UnexpectedChar

-- String prefix matcher
matchPrefixP :: String -> Parser ()
matchPrefixP s = sequence_ $ matchCharP <$> s

matchCharMapP :: Char -> a -> Parser a
matchCharMapP c a = a <$ matchCharP c

matchPrefixMapP :: String -> a -> Parser a
matchPrefixMapP s a = a <$ matchPrefixP s

digitP :: Parser Int
digitP = matchCharMapP '0' 0
     <|> matchCharMapP '1' 1
     <|> matchCharMapP '2' 2
     <|> matchCharMapP '3' 3
     <|> matchCharMapP '4' 4
     <|> matchCharMapP '5' 5
     <|> matchCharMapP '6' 6
     <|> matchCharMapP '7' 7
     <|> matchCharMapP '8' 8
     <|> matchCharMapP '9' 9

intP :: Parser Int
intP = do digits <- manyP digitP
          if digits == []
          then dirty $ ExpectedLexeme "int"
          else pure $ sum $ powerList10 0 $ reverse digits

whitespaceP :: Parser ()
whitespaceP = matchCharP ' '

skipWhitespaceP :: Parser ()
skipWhitespaceP = () <$ manyP whitespaceP
