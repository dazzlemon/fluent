{-# LANGUAGE DeriveDataTypeable #-}
module Lexer where

import Data.Data (Typeable, Data)
import Data.Char (isAlpha, isNumber, isAlphaNum, isSpace)
import Data.List (find)
import Data.Maybe (listToMaybe, isJust)
import Control.Applicative (liftA2)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Control.Monad (void, unless)

import ParserM

lexer :: String -> Either (Int, LexerError) [TokenInfo]
lexer code = evalStateT lexer' (0, code)

lexer' :: Sublexer [TokenInfo]
lexer' = do
  res <- manySublexer0 getToken
  state <- get
  let test = runStateT getToken state
  case test of
    Left (_, UnexpectedEOF _) -> return res
    Left l -> throw l

data Token = Number { tokenString::String } -- 125; 123.45; -1; -12.3; .1; -.23
           -- any sequence of characters between single quotes(')
           | StringLiteral { tokenString::String }
           | Id { tokenString::String } -- begins with alpha, and then sequence
                                        -- of alphaNum or '_' eg. aboba1_Example
           | AssignmentOperator         -- '<-'
           | ParenthesisLeft            -- '('
           | ParenthesisRight           -- ')'
           | BraceLeft                  -- '{'
           | BraceRight                 -- '}'
           | BracketLeft                -- '['
           | BracketRight               -- ']'
           | Semicolon                  -- ';'
           | WildCard                   -- '_'
           | Null                       -- 'NULL'
           | MatchKeyword               -- 'match'
           | MatchArrow                 -- '->'
           | NamedTuppleAccessOperator  -- ':'
           | NamedTuppleBindingOperator -- '='
           deriving (Show, Data, Eq)

data TokenInfo = TokenInfo { position::Int, token::Token } deriving (Eq, Show)
data LexerError = UnknownSymbol
                | UnexpectedSymbol { expected::String }
                | UnexpectedEOF    { expected::String }
                deriving (Show, Data, Eq)

getToken :: Sublexer TokenInfo
getToken = do
  (pos, tokens) <- get
  if null tokens
    then throw (pos, UnexpectedEOF "expected token")
    else do
      manySublexer0 skipNoop
      token <- foldl1 chooseParserM [ parseNumber
                                    , stringToken "match" MatchKeyword
                                    , stringToken "NULL" Null
                                    , parseId
                                    , stringToken "->" MatchArrow
                                    , stringToken "<-" AssignmentOperator
                                    , parseString
                                    , charToken '(' ParenthesisLeft
                                    , charToken ')' ParenthesisRight
                                    , charToken '{' BraceLeft
                                    , charToken '}' BraceRight
                                    , charToken '[' BracketLeft
                                    , charToken ']' BracketRight
                                    , charToken ';' Semicolon
                                    , charToken '_' WildCard
                                    , charToken ':' NamedTuppleAccessOperator
                                    , charToken '=' NamedTuppleBindingOperator
                                    ]
      (pos, _) <- get
      return $ TokenInfo pos token

parseString :: Sublexer Token
parseString = do
  char '\''
  innerString <- many0p (/= '\'')
  char '\''
  return (StringLiteral innerString)

charToken :: Char -> Token -> Sublexer Token
charToken c t = do
  char c
  return t

skipNoop :: Sublexer ()
skipNoop = chooseParserM skipComment skipWhitespace

skipComment :: Sublexer ()
skipComment = do
  char '#'
  many0p (/= '\n')
  return ()

skipWhitespace :: Sublexer ()
skipWhitespace = void $ charP "skipWhitespace" isSpace

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore = liftA2 (||) isAlphaNum (== '_')

type Sublexer a = ParserM Char a LexerError

parseId :: Sublexer Token
parseId = do
  c <- charP "id" isAlpha
  cs <- many0p isAlphaNumOrUnderscore
  return $ Id (c:cs)

stringToken :: String -> Token -> Sublexer Token
stringToken str tok = do
  string str
  return tok

parseNumber :: Sublexer Token
parseNumber = do
  isNegative <- optionalChar '-'
  let mbMinus = if isNegative
        then "-"
        else ""
      uintStr = many1p isNumber
      uint = do
        str <- uintStr
        return (Number (mbMinus ++ str))
      decimal = do
        lhs <- many0p isNumber
        let lhs' = if null lhs then "0"
                               else lhs
        char '.'
        rhs <- uintStr
        return (Number (mbMinus ++ lhs' ++ "." ++ rhs))
  chooseParserM uint decimal

optionalSublexer_ :: Sublexer a -> Sublexer ()
optionalSublexer_ = void . optionalSublexer

optionalSublexerB :: Sublexer a -> Sublexer Bool
optionalSublexerB = (isJust <$>) . optionalSublexer

optionalSublexer :: Sublexer a -> Sublexer (Maybe a)
optionalSublexer subl = do
  state <- get
  let res = runStateT subl state
  case res of
    Left _ -> return Nothing
    Right (res', state') -> do
      put state'
      return $ Just res'

manySublexer0 :: Sublexer a -> Sublexer [a]
manySublexer0 subl = do
  state <- get
  let res = runStateT subl state
  case res of
    Right (r, state') -> do
      put state'
      rs <- manySublexer0 subl
      return (r:rs)
    _ -> return []

type CharP = (Char -> Bool)

many1p :: CharP -> Sublexer String
many1p pred = do
  c <- charP "many1p" pred
  cs <- many0p pred
  return (c:cs)

many0p :: CharP -> Sublexer String
many0p pred = do
  mbC <- optionalCharP pred
  case mbC of
    Just c -> do
      cs <- many0p pred
      return (c:cs)
    _ -> return ""

optionalChar :: Char -> Sublexer Bool
optionalChar c = do
  mbC <- optionalCharP (== c)
  return $ isJust mbC

optionalCharP :: CharP -> Sublexer (Maybe Char)
optionalCharP pred = do
  (pos, str) <- get
  case listToMaybe str of
    Just c | pred c -> do
      put (pos + 1, tail str)
      return $ Just c
    _ -> return Nothing

string :: String -> Sublexer ()
string = traverse_ char

char :: Char -> Sublexer ()
char c = do
  _ <- charP ("expected " ++ [c]) (== c)
  return ()

charP :: String -> (Char -> Bool) -> Sublexer Char
charP errmsg pred = do
  (pos, str) <- get
  case listToMaybe str of
    Just c -> if pred c
      then do
        put (pos + 1, tail str)
        return c
      else throw (pos, UnexpectedSymbol errmsg)
    _ -> throw (pos, UnexpectedEOF errmsg)