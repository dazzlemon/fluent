{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Lexer
import Data.List (maximumBy, minimumBy)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Data (Typeable, Data, Constr, toConstr)
import Data.Maybe (listToMaybe)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

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
          | FunctionCall { fname::ExprPos, args::[ExprPos] }
--        | '_'
          | WildCardExpr
--        | "NULL"
          | NullExpr
--        | namedTupleAcess
          | NamedTuppleAccess { tupleName::ExprPos, fieldName::ExprPos }
--        | lambdaDef
-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
          | LambdaDef { argNames::[ExprPos], commandList::[ExprPos] }
          | Assignment { lhs::ExprPos, rhs::ExprPos }
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
          | NamedTuple { keyValuePairs::[(ExprPos, ExprPos)] }
          | Empty -- kinda like TokenEOF
          deriving (Show, Data, Eq)

newtype ParserError = ParserError String deriving (Show, Data, Eq)

--                  pos  rest
type ParserState = (Int, [Token])

-- type Subparser a = ParserState
-- --                      (unexpectedTokenOffset, err) (res, newState)
--               -> Either (Int,           ParserError) (a, ParserState)
type Subparser a = StateT ParserState (Either (Int, ParserError)) a


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
parseCommand = do
  (pos, tokens) <- get
  if null tokens
    then return (Empty, pos)
    else do
      let subparsers = [ parseAssignment
                       , parseFunctionCall
                       , parsePatternMatching
                       ]
          parseResultsEither = map (\f -> runStateT f (pos, tokens)) subparsers
          (parseErrors, parseGood) = partitionEithers parseResultsEither
          furthestError = maximumBy (compare `on` fst) parseErrors
          furthestGood = minimumBy (compare `on` (fst . snd)) parseGood
          returnGood = do
            let (res, state) = furthestGood
            put state
            return res
      case (parseErrors, parseGood) of
        -- only errors -> furthest error
        (_ , []) -> throw $ if fst furthestError == pos
          then (pos, ParserError "Unknown construction")
          else furthestError
        -- no errors -> furthest good
        ([], _ ) -> returnGood
        -- some errors, and some good -> just return furthest
        (_ , _ ) -> if fst (snd furthestGood) > fst furthestError
            then returnGood
            else throw furthestError

throw = lift . Left

-- assignment ::= id "<-" expr
parseAssignment :: Subparser ExprPos
parseAssignment = do
  (pos, _) <- get
  string <- skipId
  skipToken "assignment" AssignmentOperator
  rhs <- parseExpr
  return (Assignment (ExprId string, pos) rhs, pos)

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
  return (FunctionCall (ExprId strId, pos) args, pos)

parseFunctionArgs :: Subparser [ExprPos]
parseFunctionArgs = parseFunctionArgs' []

parseFunctionArgs' :: [ExprPos] -> Subparser [ExprPos]
parseFunctionArgs' args = do
  (pos, tokens) <- get
  case listToMaybe tokens of
    Just ParenthesisRight -> do
      put (pos + 1, tail tokens)
      return args
    _ -> do
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
parseMatchBody' cases = do
  (pos, tokens) <- get
  case tokens of
    (WildCard:MatchArrow:rest) -> do
      put (pos + 2, rest)
      defaultCase <- parseExpr
      skipToken "matchBody" Semicolon
      skipToken "matchBody" BraceRight
      return (cases, defaultCase)
    _ -> do
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
parseExpr = do
  (pos, tokens) <- get
  if null tokens
    then throw (pos, ParserError "parseExpr empty")
    else do
      let subparsers = [ parseFunctionCall
                        , parseNamedTupleAcess
                        , parseLambdaDef
                        , parseNamedTuple
                        , parseTuple
                        ]
          parseResultsEither = map (\f -> runStateT f (pos, tokens)) subparsers
          (parseErrors, parseGood) = partitionEithers parseResultsEither
          furthestError = maximumBy (compare `on` fst) parseErrors
          furthestGood = minimumBy (compare `on` (fst . snd)) parseGood
          furthest = if fst (snd furthestGood) > fst furthestError
            then Right furthestGood
            else Left furthestError
          -- single token cases
          singleTokenExpr = case head tokens of
            Number number -> Just $ ExprNumber number
            StringLiteral string -> Just $ ExprString string
            Id id -> Just $ ExprId id
            WildCard -> Just WildCardExpr
            Null -> Just NullExpr
            _ -> Nothing
          returnGood = do
            let (res, state) = furthestGood
            put state
            return res
      case (parseErrors, parseGood) of
        -- only errors from complex expressions ->
        -- if error is on first token we might still have singleTokenExpr
        (_ , []) -> case singleTokenExpr of
          Just expr -> do
            put (pos + 1, tail tokens)
            return (expr, pos)
          _ -> throw furthestError
        -- no errors -> furthest good
        ([], _ ) -> returnGood
        -- some errors, and some good -> just return furthest
        (_ , _ ) -> if fst (snd furthestGood) > fst furthestError
            then returnGood
            else throw furthestError

-- namedTupleAcess ::= id ':' id
parseNamedTupleAcess :: Subparser ExprPos
parseNamedTupleAcess = do
  (pos, _) <- get
  lhs <- skipId
  skipToken "named tuple acess" NamedTuppleAccessOperator
  (pos2, _) <- get
  rhs <- skipId
  return (NamedTuppleAccess (ExprId lhs, pos) (ExprId rhs, pos2), pos)

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

parseLambdaArgs :: Subparser [ExprPos]
parseLambdaArgs = parseLambdaArgs' []

parseLambdaArgs' :: [ExprPos] -> Subparser [ExprPos]
parseLambdaArgs' args = do
  (pos, tokens) <- get
  case listToMaybe tokens of
    Just ParenthesisRight -> do
      put (pos + 1, tail tokens)
      return args
    Just (Id arg) -> do
      put (pos + 1, tail tokens)
      parseLambdaArgs' (args ++ [(ExprId arg, pos)])
    _ -> throw (pos, ParserError "parseLambdaArgs' error")

parseLambdaBody :: Subparser [ExprPos]
parseLambdaBody = parseLambdaBody' []

parseLambdaBody' :: [ExprPos] -> Subparser [ExprPos]
parseLambdaBody' commands = do
  (pos, tokens) <- get
  case tokens of
    (BraceRight:rest) -> do
      put (pos + 1, rest)
      return commands
    _ -> do
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

parseNamedTupleFields :: Subparser [(ExprPos, ExprPos)]
parseNamedTupleFields = parseNamedTupleFields' []

parseNamedTupleFields' :: [(ExprPos, ExprPos)] -> Subparser [(ExprPos, ExprPos)]
parseNamedTupleFields' fields = do
  (pos, tokens) <- get
  case tokens of
    (BracketRight:rest) -> do
      put (pos + 1, rest)
      return fields
    _ -> do
      (lhs, rhs) <- parseNamedTupleField
      parseNamedTupleFields' (fields ++ [(lhs, rhs)])

parseNamedTupleField :: Subparser (ExprPos, ExprPos)
parseNamedTupleField = do
  (pos, _) <- get
  lhs <- skipId
  skipToken "named tuple field" NamedTuppleBindingOperator
  rhs <- parseExpr
  return ((ExprId lhs, pos), rhs)

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
parseTupleFields' fields = do
  (pos, tokens) <- get
  case tokens of
    (BracketRight:rest) -> do
      put (pos + 1, rest)
      return fields
    _ -> do
      field <- parseExpr
      parseTupleFields' (fields ++ [field])