{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import ParserM

import Lexer (Token( Id
                   , Number
                   , StringLiteral
                   , WildCard
                   , Null
                   , Semicolon
                   , AssignmentOperator
                   , ParenthesisLeft
                   , ParenthesisRight
                   , MatchKeyword
                   , MatchArrow
                   , BraceLeft
                   , BraceRight
                   , BracketLeft
                   , BracketRight
                   , NamedTuppleAccessOperator
                   , NamedTuppleBindingOperator
                   )
             )
import Data.List (maximumBy, minimumBy)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Data (Typeable, Data, Constr, toConstr)
import Data.Maybe (listToMaybe)
import Control.Monad.Trans.State

-- int represents # of token, needed for better error messages in Evaluator.hs
type ExprPos = (Expr, Int)

-- program ::= { command ';'}
type Program = [ExprPos]

-- expr ::= number
data Expr = ExprNumber { str::String }
--        | string
          | ExprString { str::String }
--        | id
          | ExprId { str::String }
--        | functionCall
  -- functionCall ::= id '(' {expr} ')'
          | FunctionCall { fname::String, args::[ExprPos] }
--        | '_'
          | WildCardExpr
--        | "NULL"
          | NullExpr
--        | namedTupleAcess
          | NamedTuppleAccess { tupleName::String, fieldName::String }
--        | lambdaDef
-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
          | LambdaDef { argNames::[String], commandList::[ExprPos] }
          | Assignment { lhs::String, rhs::ExprPos }
  -- patternMatching ::= 'match' expr '{'
  --   {expr '->' expr ';'}
  --   '_' '->' expr ';' '}'
          | PatternMatching { switch::ExprPos
                            , cases::[(ExprPos, ExprPos)]
                            , defaultCase::ExprPos
                            }
-- tuple ::= '[' {expr} ']'
          | Tuple { values::[ExprPos] }
-- namedTupleField ::= id '=' expr
-- namedTuple ::= '[' {namedTupleField} ']'
          | NamedTuple { keyValuePairs::[(String, ExprPos)] }
          deriving (Show, Data, Eq)

newtype ParserError = ParserError String deriving (Show, Data, Eq)

type Subparser a = ParserM Token a ParserError

parser :: [Token] -> Either (Int, ParserError) Program
parser tokens = evalStateT (parser' []) (0, tokens)

-- program ::= { command ';'}
parser' :: Program -> Subparser Program
parser' commands = do
  (pos, tokens) <- get
  if null tokens
    then return commands
    else do
      command <- parseCommand
      skipToken "command" Semicolon
      parser' (commands ++ [command])

-- command ::= assignment | functionCall | patternMatching
parseCommand :: Subparser ExprPos
parseCommand = foldl1 chooseParserM [ parseAssignment
                                    , parseFunctionCall
                                    , parsePatternMatching
                                    ]

-- assignment ::= id "<-" expr
parseAssignment :: Subparser ExprPos
parseAssignment = do
  (pos, _) <- get
  string <- skipId
  skipToken "assignment" AssignmentOperator
  rhs <- parseExpr
  return (Assignment string rhs, pos)

skipId :: Subparser String
skipId = do
  (pos, tokens) <- get
  case listToMaybe tokens of
    Just (Id string) -> do
      put (pos + 1, tail tokens)
      return string
    Just first -> throw (pos, error first)
    Nothing -> throw (pos, error "empty list of tokens")
  where error got =
          ParserError $ "expected `Id`, but got " ++ show (toConstr got)

-- functionCall ::= id '(' {expr} ')'
parseFunctionCall :: Subparser ExprPos
parseFunctionCall = do
  (pos, _) <- get
  strId <- skipId
  skipToken "functionCall" ParenthesisLeft
  args <- parseFunctionArgs
  return (FunctionCall strId args, pos)

parseFunctionArgs :: Subparser [ExprPos]
parseFunctionArgs = parseFunctionArgs' []

parseFunctionArgs' :: [ExprPos] -> Subparser [ExprPos]
parseFunctionArgs' args = chooseParserM end continue
  where end = do
          skipToken "functiona call args end" ParenthesisRight
          return args
        continue = do
          arg <- parseExpr
          parseFunctionArgs' (args ++ [arg])

-- patternMatching ::= 'match' expr '{'
-- 	{expr '->' expr ';'}
-- 	'_' '->' expr ';' '}'
parsePatternMatching :: Subparser ExprPos
parsePatternMatching = do
  (pos, _) <- get
  skipToken "patternMatch" MatchKeyword
  switch <- parseExpr
  skipToken "patternMatch" BraceLeft
  (cases, defaultCase) <- parseMatchBody
  return (PatternMatching switch cases defaultCase, pos)

parseMatchBody :: Subparser ([(ExprPos, ExprPos)], ExprPos)
parseMatchBody = parseMatchBody' []

parseMatchBody' :: [(ExprPos, ExprPos)] -> Subparser ([(ExprPos, ExprPos)], ExprPos)
parseMatchBody' cases = chooseParserM end continue
  where end = do
          skipToken "pattern match body end" WildCard
          skipToken "pattern match body end" MatchArrow
          defaultCase <- parseExpr
          skipToken "matchBody" Semicolon
          skipToken "matchBody" BraceRight
          return (cases, defaultCase)
        continue = do
          lhs <- parseExpr
          skipToken "matchBody" MatchArrow
          rhs <- parseExpr
          skipToken "matchBody" Semicolon
          parseMatchBody' (cases ++ [(lhs, rhs)])

skipToken :: String -> Token -> Subparser ()
skipToken fname token = do
  (pos, tokens) <- get
  case listToMaybe tokens of
    Just first -> if first == token
      then do
        put (pos + 1, tail tokens)
        return ()
      else throw (pos, error $ "`" ++ showConstr first ++ "`")
    Nothing -> throw (pos, error "empty list of tokens")
  where error suffix = ParserError $ fname ++ ": expected `" ++ showConstr token
                                  ++ "`, but got " ++ suffix
        showConstr = show . toConstr

-- expr ::= number
--        | string
--        | id
--        | functionCall
--        | '_'
--        | "NULL"
--        | namedTupleAcess
--        | lambdaDef
--        | namedTuple
--        | tuple
parseExpr :: Subparser ExprPos
parseExpr = foldl1 chooseParserM [ parseFunctionCall
                                 , parseNamedTupleAcess
                                 , parseLambdaDef
                                 , parseNamedTuple
                                 , parseTuple
                                 , singleTokenExpr
                                 ]

singleTokenExpr :: Subparser ExprPos
singleTokenExpr = do
  (pos, tokens) <- get
  case listToMaybe tokens of
    Just x -> do
      let maybeExpr = case x of
            Number n -> Just $ ExprNumber n
            StringLiteral s -> Just $ ExprString s
            Id id -> Just $ ExprId id
            WildCard -> Just WildCardExpr
            Null -> Just NullExpr
            _ -> Nothing
      case maybeExpr of
        Nothing -> throw (pos, ParserError "expected simple expression")
        Just expr -> do
          put (pos + 1, tail tokens)
          return (expr, pos)
    _ -> throw (pos, ParserError "unexpected end of token stream")

-- namedTupleAcess ::= id ':' id
parseNamedTupleAcess :: Subparser ExprPos
parseNamedTupleAcess = do
  (pos, _) <- get
  lhs <- skipId
  skipToken "named tuple acess" NamedTuppleAccessOperator
  rhs <- skipId
  return (NamedTuppleAccess lhs rhs, pos)

-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
parseLambdaDef :: Subparser ExprPos
parseLambdaDef = do
  (pos, _) <- get
  skipToken "lambda definition" ParenthesisLeft
  args <- parseLambdaArgs
  skipToken "lambda definition" BraceLeft
  body <- parseLambdaBody
  return (LambdaDef args body, pos)

parseLambdaArgs :: Subparser [String]
parseLambdaArgs = parseLambdaArgs' []

parseLambdaArgs' :: [String] -> Subparser [String]
parseLambdaArgs' args = chooseParserM end continue
  where end = do
          skipToken "lambda args end" ParenthesisRight
          return args
        continue = do
          (pos, _) <- get
          arg <- skipId
          parseLambdaArgs' (args ++ [arg])

parseLambdaBody :: Subparser [ExprPos]
parseLambdaBody = parseLambdaBody' []

parseLambdaBody' :: [ExprPos] -> Subparser [ExprPos]
parseLambdaBody' commands = chooseParserM end continue
  where end = do
          skipToken "lambda body end" BraceRight
          return commands
        continue = do
          command <- parseCommand
          skipToken "lambda body" Semicolon
          parseLambdaBody' (commands ++ [command])

-- namedTuple ::= '[' {namedTupleField} ']'
-- namedTupleField ::= id '=' expr
parseNamedTuple :: Subparser ExprPos
parseNamedTuple = do
  (pos, _) <- get
  skipToken "named tuple" BracketLeft
  fields <- parseNamedTupleFields
  return (NamedTuple fields, pos)

parseNamedTupleFields :: Subparser [(String, ExprPos)]
parseNamedTupleFields = parseNamedTupleFields' []

parseNamedTupleFields' :: [(String, ExprPos)] -> Subparser [(String, ExprPos)]
parseNamedTupleFields' fields = chooseParserM end continue
  where end = do
          skipToken "tuple end" BracketRight
          return fields
        continue = do
          (lhs, rhs) <- parseNamedTupleField
          parseNamedTupleFields' (fields ++ [(lhs, rhs)])

parseNamedTupleField :: Subparser (String, ExprPos)
parseNamedTupleField = do
  (pos, _) <- get
  lhs <- skipId
  skipToken "named tuple field" NamedTuppleBindingOperator
  rhs <- parseExpr
  return (lhs, rhs)

-- tuple ::= '[' {expr} ']'
parseTuple :: Subparser ExprPos
parseTuple = do
  (pos, _) <- get
  skipToken "tuple" BracketLeft
  fields <- parseTupleFields
  return (Tuple fields, pos)

parseTupleFields :: Subparser [ExprPos]
parseTupleFields = parseTupleFields' []

parseTupleFields' :: [ExprPos] -> Subparser [ExprPos]
parseTupleFields' fields = chooseParserM end continue
  where end = do
          skipToken "tuple end" BracketRight
          return fields
        continue = do
          field <- parseExpr
          parseTupleFields' (fields ++ [field])