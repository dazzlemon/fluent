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
getToken code position =
  case runStateT (foldl1 chooseSublexer [ parseNumber
                                        , parseMatchArrow
                                        , parseAssignmentOperator
                                        , parseId
                                        ]
                 ) (position, code) of
    Right (t, (pos, rest)) -> Right (TokenInfo position t, rest)
    _ -> getToken' code position

getToken' :: String -> Int -> Either LexerError (TokenInfo, String)
getToken' [] position = Right (TokenInfo position TokenEOF, [])
getToken' (firstChar:code) position
  | Just token <- charToToken firstChar = returnToken token code
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
  | otherwise = Left (UnknownSymbol position)
  where returnToken token rest = Right (TokenInfo position token, rest)

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore = liftA2 (||) isAlphaNum (== '_')

-- new

type LexerState = (Int, String)
type Sublexer a = StateT LexerState (Either (Int, LexerError)) a

throw = lift . Left

parseId :: Sublexer Token
parseId = do
  c <- charP "id" isAlpha
  cs <- many0p isAlphaNumOrUnderscore
  let identifier = c:cs
  return $ case wordToToken identifier of -- TODO: make separate parsers
    Just token -> token
    _ -> Id identifier

parseAssignmentOperator :: Sublexer Token
parseAssignmentOperator = do
  string "<-"
  return AssignmentOperator

parseMatchArrow :: Sublexer Token
parseMatchArrow = do
  string "->"
  return MatchArrow

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
  chooseSublexer uint decimal

chooseSublexer :: Sublexer a -> Sublexer a -> Sublexer a
chooseSublexer p1 p2 = do
  state <- get
  let r1 = runStateT p1 state
  let r2 = runStateT p2 state
  case (r1, r2) of
    (Left l1, Left l2) -> throwFurthest l1 l2
    (Right g1, Right g2) -> returnFurthest g1 g2
    (Left l, Right g) -> returnOrThrowFurthest g l
    (Right g, Left l) -> returnOrThrowFurthest g l
  where throwFurthest (p1, e1) (p2, e2) = throw $ if p1 >= p2
          then (p1, e1)
          else (p2, e2)
        return' (a, (d, t)) = do
          put (d, t)
          return a
        returnFurthest (a1, (d1, t1)) (a2, (d2, t2)) = return' $ if d1 >= d2
          then (a1, (d1, t1))
          else (a2, (d2, t2))
        returnOrThrowFurthest (a, (d, t)) (p, e) = if d >= p
          then return' (a, (d, t))
          else throw (p, e)

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

charP :: String -> CharP -> Sublexer Char
charP errmsg pred = do
  (pos, str) <- get
  case listToMaybe str of
    Just c -> if pred c
      then do
        put (pos + 1, tail str)
        return c
      else throw (pos, UnexpectedSymbol pos errmsg)
    _ -> throw (pos, UnexpectedEOF errmsg)
