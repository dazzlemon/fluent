{-# LANGUAGE DeriveDataTypeable #-}
module Lexer where

import Data.Data (Typeable, Data)
import Data.Char (isAlpha, isNumber, isAlphaNum, isSpace)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Control.Applicative (liftA2)

lexer :: String -> Either LexerError [TokenInfo]
lexer code = lexer' code 0 []

lexer' :: String -> Int -> [TokenInfo] -> Either LexerError [TokenInfo]
lexer' [] _ tokens = Right tokens
lexer' code position tokens = case getToken code position of
  -- repeat until TokenEOF or err
  Right (tokenInfo@(TokenInfo position token), rest) -> case token of
    TokenEOF -> Right tokens
    _ -> lexer' rest (position + tokenLength token) (tokens ++ [tokenInfo])
  Left err -> Left err

tokenLength :: Token -> Int
tokenLength tokenNumber = case tokenNumber of
  Number number -> length number
  StringLiteral string -> length string + 2 -- +2 for quotes
  Id id -> length id
  AssignmentOperator -> 2
  Null -> 4
  MatchKeyword -> 5
  MatchArrow -> 2
  _ -> 1

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
           | TokenEOF
           deriving (Show, Data, Eq)

data TokenInfo = TokenInfo { position::Int, token::Token } deriving (Eq, Show)
data LexerError = UnknownSymbol    { errorPosition::Int }
                | UnexpectedSymbol { errorPosition::Int
                                   , expected::String }
                | UnexpectedEOF    { expected::String }
                deriving (Show, Data, Eq)

charToToken :: Char -> Maybe Token
charToToken = flip lookup [ ('(', ParenthesisLeft)
                          , (')', ParenthesisRight)
                          , ('{', BraceLeft)
                          , ('}', BraceRight)
                          , ('[', BracketLeft)
                          , (']', BracketRight)
                          , (';', Semicolon)
                          , ('_', WildCard)
                          , (':', NamedTuppleAccessOperator)
                          , ('=', NamedTuppleBindingOperator)
                          ]

wordToToken :: String -> Maybe Token
wordToToken = flip lookup [ ("match", MatchKeyword)
                          , ("NULL", Null)
                          ]

getToken :: String -> Int -> Either LexerError (TokenInfo, String)
getToken [] position = Right (TokenInfo position TokenEOF, [])
getToken (firstChar:code) position
  | Just token <- charToToken firstChar = returnToken token code
  | firstChar == '-' = case listToMaybe code of
    -- TODO: cleanup this branch
    Just '>' -> returnToken MatchArrow (tail code)
    -- +1 because skipping '-'
    Just x | isNumber x || x == '.' -> case getNumber code (position + 1) of
      -- don't care for the position, because it starts where we said it to
      Right (TokenInfo _ (Number positive), rest) ->
        Right (TokenInfo position (Number ('-':positive)), rest)
      Left (UnexpectedSymbol position' err) ->
        Left (UnexpectedSymbol position' (err ++ suffix))
      _ -> Left (UnexpectedSymbol position ("numeric" ++ suffix))
    Just x -> Left (UnexpectedSymbol (position + 1) ("numeric" ++ suffix))
    _ -> Left $ UnexpectedEOF "numeric or '>'"
  | firstChar == '#' = case span (/= '\n') code of
    -- +2 for '\n' and '#'
    (comment, _:restAfterComment) ->
      getToken restAfterComment (position + length comment + 2)
    (_, []) -> returnToken TokenEOF []
  | isSpace firstChar = getToken code (position + 1) -- +1 -> skip char
  | firstChar == '\'' = case span (/= '\'') code of
    (string, _:restAfterString) ->
      returnToken (StringLiteral string) restAfterString
    (_, []) -> Left $ UnexpectedEOF "\'" -- no closing quote
  | isNumber firstChar || firstChar == '.' = getNumber (firstChar:code) position
  | isAlpha firstChar = returnToken token restAfterId
  -- +1 because skip '.'
  | firstChar == '<' = case listToMaybe code of
    Just '-' -> returnToken AssignmentOperator (tail code)
    Just x -> Left $ UnexpectedSymbol (position + 1) "-" -- +1 skip '<'
    _ -> Left $ UnexpectedEOF "-"
  | otherwise = Left (UnknownSymbol position)
  where (idRest, restAfterId) = span isAlphaNumOrUnderscore code
        identifier = firstChar:idRest
        token = case wordToToken identifier of
          Just token -> token
          _ -> Id identifier
        returnToken token rest = Right (TokenInfo position token, rest)
        suffix = case listToMaybe code of
          Just '.' -> ""
          _ -> " or '>'"

-- only accepts positive numbers in formats like: 1, 1.2, .2
getNumber :: String -> Int -> Either LexerError (TokenInfo, String)
getNumber code position = case afterWhole of
  "." -> Left $ UnexpectedEOF "numeric"
  '.':_ -> if null fraction
    then Left (UnexpectedSymbol (position + length whole + 1) "numeric")
    else Right (TokenInfo position (Number (whole ++ "." ++ fraction)), afterFraction)
  _ -> Right (TokenInfo position (Number whole), afterWhole)
  where (whole, afterWhole) = span isNumber code
        (fraction, afterFraction) = span isNumber
                                  $ drop 1 afterWhole -- drop '.'

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore = liftA2 (||) isAlphaNum (== '_')