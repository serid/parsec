module JitLib.Json where

import Control.Applicative
import qualified Data.Map as Map

import JitLib.Parsec
import JitLib.Result
import JitLib.Util

data JsValue = JsNull
             | JsBool Bool
             | JsInt Int
             | JsString String
             | JsArray [JsValue]
             | JsObject (Map.Map String JsValue)

parseJs :: String -> Result ParsingError JsValue
parseJs text = do (trail, value) <- runParser jsValue text
                  if trail == []
                  then Ok value
                  else Err $ TrailingCharacters trail

jsValue :: Parser JsValue
jsValue = jsNull <|> jsBool <|> jsInt <|> jsString <|> jsArray <|> jsObject

jsNull :: Parser JsValue
jsNull = JsNull <$ matchPrefixP "null"

jsBool :: Parser JsValue
jsBool = JsBool False <$ matchPrefixP "false"
     <|> JsBool True <$ matchPrefixP "true"

jsInt :: Parser JsValue
jsInt = JsInt <$> intP

jsString :: Parser JsValue
jsString = JsString <$> jsString'

jsString' :: Parser String
jsString' = do matchCharP '"'
               string <- manyP charInsideStringP
               matchCharP '"'
               pure $ string
    where
        charInsideStringP :: Parser Char
        charInsideStringP = do c <- charP
                               if c == '"'
                               then dirty $ Undef
                               else pure $ c

jsArray :: Parser JsValue
jsArray = do matchCharP '['
             skipWhitespaceP
             array <- manyP valueAndMaybeCommaInsideArray
             matchCharP ']'
             pure $ JsArray $ array
    where
        valueAndMaybeCommaInsideArray :: Parser JsValue
        valueAndMaybeCommaInsideArray =
            do value <- jsValue
               skipWhitespaceP
               maybeParseOrMaybeDon't $ matchCharP ',' -- Consume a comma if there is one
               skipWhitespaceP
               pure $ value

jsObject :: Parser JsValue
jsObject = do skipWhitespaceP
              matchCharP '{'
              pairs <- manyP valueAndMaybeCommaInsideObject
              matchCharP '}'
              pure $ JsObject $ Map.fromList pairs
    where
        valueAndMaybeCommaInsideObject :: Parser (String, JsValue)
        valueAndMaybeCommaInsideObject =
            do key <- jsString'
               skipWhitespaceP
               matchCharP ':'
               skipWhitespaceP
               value <- jsValue
               skipWhitespaceP
               maybeParseOrMaybeDon't $ matchCharP ',' -- Consume a comma if there is one
               skipWhitespaceP
               pure $ (key, value)

instance Show JsValue where
    show (JsNull) = "null"
    show (JsBool False) = "false"
    show (JsBool True) = "true"
    show (JsInt n) = show n
    show (JsString s) = show s
    show (JsArray a) = "[" ++ (foldMapSep ", " (\v -> show v) a) ++ "]"
    show (JsObject m) = "{" ++ (foldMapSep ", " (\(k, v) -> show k ++ ": " ++ show v) $ Map.toList m) ++ "}"

jsToText :: JsValue -> String
jsToText = show
