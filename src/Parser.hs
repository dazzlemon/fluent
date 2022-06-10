{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Lexer
import Data.List (maximumBy, minimumBy)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Data (Typeable, Data, Constr, toConstr)
import Data.Maybe (listToMaybe)

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

type Subparser = ParserState
--                      (unexpectedTokenOffset, err) (subAST, newState)
              -> Either (Int,           ParserError) (ExprPos, ParserState)
 
parser :: [Token] -> Either (Int, ParserError) Program
parser tokens = parser' (0, tokens) []

-- program ::= { command ';'}
parser' :: ParserState -> Program -> Either (Int, ParserError) Program
parser' (pos, []) commands = return commands
parser' state commands = do
  (command, state1) <- parseCommand state
  state2 <- skipToken "command" Semicolon state1
  parser' state2 (commands ++ [command])

-- command ::= assignment | functionCall | patternMatching
parseCommand :: Subparser
parseCommand (pos, []) = Right ((Empty, pos), (pos, []))
parseCommand (pos, tokens) = case (parseErrors, parseGood) of
  -- only errors -> furthest error
  (_ , []) -> if fst furthestError == pos
    then Left (pos, ParserError "Unknown construction")
    else Left furthestError
  -- no errors -> furthest good
  ([], _ ) -> Right furthestGood
  -- some errors, and some good -> just return furthest
  (_ , _ ) -> furthest
  where subparsers = [ parseAssignment
                     , parseFunctionCall
                     , parsePatternMatching
                     ]
        parseResultsEither = map (\f -> f (pos, tokens)) subparsers
        (parseErrors, parseGood) = partitionEithers parseResultsEither
        furthestError = maximumBy (compare `on` fst) parseErrors
        furthestGood = minimumBy (compare `on` (length . snd)) parseGood
        furthestGoodDist = ((-) `on` length) tokens (snd $ snd furthestGood) + pos
        furthest = if furthestGoodDist > fst furthestError
          then Right furthestGood
          else Left furthestError


-- assignment ::= id "<-" expr
parseAssignment :: Subparser
parseAssignment state = do
  (string, state1) <- skipId state
  state2 <- skipToken "assignment" AssignmentOperator state1
  (rhs, newState) <- parseExpr state2
  return ((Assignment (ExprId string, pos) rhs, pos), newState)
  where pos = fst state

skipId :: ParserState -> Either (Int, ParserError) (String, ParserState)
skipId (pos, tokens) = case listToMaybe tokens of
  Just (Id string) -> return (string, (pos + 1, tail tokens))
  Just first -> Left (pos, error first)
  Nothing -> Left (pos, error "empty list of tokens")
  where error got =
          ParserError $ "expected `Id`, but got " ++ show (toConstr got)

-- functionCall ::= id '(' {expr} ')'
parseFunctionCall :: Subparser
parseFunctionCall state = do
  (strId, state1) <- skipId state
  state2 <- skipToken "functionCall" ParenthesisLeft state1
  (args, state3) <- parseFunctionArgs state2
  return ((FunctionCall (ExprId strId, pos) args, pos), state3)
  where pos = fst state

parseFunctionArgs :: ParserState
                  -> Either (Int, ParserError) ([ExprPos], ParserState)
parseFunctionArgs = parseFunctionArgs' []

parseFunctionArgs' :: [ExprPos] -> ParserState
                   -> Either (Int, ParserError) ([ExprPos], ParserState)
parseFunctionArgs' args (pos, ParenthesisRight:rest) =
  Right (args, (pos + 1, rest))
parseFunctionArgs' args state = do
  (arg, state') <- parseExpr state
  parseFunctionArgs' (args ++ [arg]) state'

-- patternMatching ::= 'match' expr '{'
-- 	{expr '->' expr ';'}
-- 	'_' '->' expr ';' '}'
parsePatternMatching :: Subparser
parsePatternMatching state = do
  state1 <- skipToken "patternMatch" MatchKeyword state
  (switch, state2) <- parseExpr state1
  state3 <- skipToken "patternMatch" BraceLeft state2
  (cases, defaultCase, state4) <- parseMatchBody state3
  return ((PatternMatching switch cases defaultCase, fst state), state4)

parseMatchBody :: ParserState
               -> Either (Int, ParserError) ([(ExprPos, ExprPos)], ExprPos, ParserState)
parseMatchBody = parseMatchBody' []

parseMatchBody' :: [(ExprPos, ExprPos)] -> ParserState
                -> Either (Int, ParserError) ([(ExprPos, ExprPos)], ExprPos, ParserState)
parseMatchBody' cases (pos, WildCard:MatchArrow:tokens) = do
  (defaultCase, state1) <- parseExpr (pos + 2, tokens)
  state2 <- skipToken "matchBody" Semicolon state1
  state3 <- skipToken "matchBody" BraceRight state2
  return (cases, defaultCase, state3)
parseMatchBody' cases state = do
  (lhs, state1) <- parseExpr state
  state2 <- skipToken "matchBody" MatchArrow state1
  (rhs, state3) <- parseExpr state2
  state4 <- skipToken "matchBody" Semicolon state3
  parseMatchBody' (cases ++ [(lhs, rhs)]) state4

skipToken :: String -> Token -> ParserState -> Either (Int, ParserError) ParserState
skipToken fname token (pos, tokens) = case listToMaybe tokens of
  Just first -> if first == token
    then Right (pos + 1, tail tokens)
    else Left (pos, error $ "`" ++ showConstr first ++ "`")
  Nothing -> Left (pos, error "empty list of tokens")
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
parseExpr :: Subparser
parseExpr (pos, []) = Left (pos, ParserError "parseExpr empty")
parseExpr state@(pos, tokens) = case (parseErrors, parseGood) of
  -- only errors from complex expressions ->
  -- if error is on first token we might still have singleTokenExpr
  (_ , []) -> case singleTokenExpr of
    Just expr -> Right ((expr, pos), (pos + 1, tail tokens))
    _ -> Left furthestError
  -- no errors -> furthest good
  ([], _ ) -> Right furthestGood
  -- some errors, and some good -> just return furthest
  (_ , _ ) -> furthest
        -- cases with subExpressions
  where subparsers = [ parseFunctionCall
                     , parseNamedTupleAcess
                     , parseLambdaDef
                     , parseNamedTuple
                     , parseTuple
                     ]
        parseResultsEither = map ($ state) subparsers
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

-- namedTupleAcess ::= id ':' id
parseNamedTupleAcess :: Subparser
parseNamedTupleAcess state = do
  (lhs, state1) <- skipId state
  state2 <- skipToken "named tuple acess" NamedTuppleAccessOperator state1
  (rhs, state3) <- skipId state2
  Right ( ( NamedTuppleAccess
            (ExprId lhs, fst state)
            (ExprId rhs, fst state3)
          , fst state
          )
        , state3
        )

-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
parseLambdaDef :: Subparser
parseLambdaDef state = do
  state1 <- skipToken "lambda definition" ParenthesisLeft state
  (args, state2) <- parseLambdaArgs state1
  state3 <- skipToken "lambda definition" BraceLeft state2
  (body, state4) <- parseLambdaBody state3
  return ((LambdaDef args body, fst state), state4)

parseLambdaArgs :: ParserState
                -> Either (Int, ParserError) ([ExprPos], ParserState)
parseLambdaArgs = parseLambdaArgs' []

parseLambdaArgs' :: [ExprPos] -> ParserState
                -> Either (Int, ParserError) ([ExprPos], ParserState)
parseLambdaArgs' args (pos, ParenthesisRight:rest) =
  Right (args, (pos + 1, rest))
parseLambdaArgs' args (pos, Id arg:rest) =
  parseLambdaArgs' (args ++ [(ExprId arg, pos)]) (pos + 1, rest)
parseLambdaArgs' _ (pos, _) = Left (pos, ParserError "parseLambdaArgs' error")

parseLambdaBody :: ParserState
                -> Either (Int, ParserError) ([ExprPos], ParserState)
parseLambdaBody = parseLambdaBody' []

parseLambdaBody' :: [ExprPos] -> ParserState
                 -> Either (Int, ParserError) ([ExprPos], ParserState)
parseLambdaBody' commands (pos, BraceRight:rest) = Right (commands, (pos + 1, rest))
parseLambdaBody' commands state = do
  (command, state1) <- parseCommand state
  state2 <- skipToken "lambda body" Semicolon state1
  parseLambdaBody' (commands ++ [command]) state2

-- namedTuple ::= '[' {namedTupleField} ']'
-- namedTupleField ::= id '=' expr
parseNamedTuple :: Subparser
parseNamedTuple state = do
  state1 <- skipToken "named tuple" BracketLeft state
  (fields, state2) <- parseNamedTupleFields state1
  return ((NamedTuple fields, fst state), state2)

parseNamedTupleFields :: ParserState
                 -> Either (Int, ParserError) ([(ExprPos, ExprPos)], ParserState)
parseNamedTupleFields = parseNamedTupleFields' []

parseNamedTupleFields' :: [(ExprPos, ExprPos)] -> ParserState
                  -> Either (Int, ParserError) ([(ExprPos, ExprPos)], ParserState)
parseNamedTupleFields' fields (pos, BracketRight:rest) = Right (fields, (pos + 1, rest))
parseNamedTupleFields' fields state = do
  (lhs, rhs, state1) <- parseNamedTupleField state
  parseNamedTupleFields' (fields ++ [(lhs, rhs)]) state1

parseNamedTupleField :: ParserState
                     -> Either (Int, ParserError) (ExprPos, ExprPos, ParserState)
parseNamedTupleField state = do
  (lhs, state1) <- skipId state
  state2 <- skipToken "named tuple field" NamedTuppleBindingOperator state1
  (rhs, state3) <- parseExpr state2
  return ((ExprId lhs, fst state), rhs, state3)

-- tuple ::= '[' {expr} ']'
parseTuple :: Subparser
parseTuple state = do
  state1 <- skipToken "tuple" BracketLeft state
  (fields, state2) <- parseTupleFields state1
  return ((Tuple fields, fst state), state2)

parseTupleFields :: ParserState
                 -> Either (Int, ParserError) ([ExprPos], ParserState)
parseTupleFields = parseTupleFields' []

parseTupleFields' :: [ExprPos] -> ParserState
                  -> Either (Int, ParserError) ([ExprPos], ParserState)
parseTupleFields' fields (pos, BracketRight:rest) = Right (fields, (pos + 1, rest))
parseTupleFields' fields state = do
  (field, state1) <- parseExpr state
  parseTupleFields' (fields ++ [field]) state1