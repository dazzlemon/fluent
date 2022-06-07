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
          | FunctionCall { id::ExprPos, args::[ExprPos] }
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

--               pos    tokens
type Subparser = Int -> [Token]
--                      (unexpectedTokenOffset, err) (subAST, rest)
              -> Either (Int,           ParserError) (ExprPos, [Token])
 
parser :: [Token] -> Either (Int, ParserError) Program
parser = parser' 0 []

-- program ::= { command ';'}
parser' :: Int -> Program -> [Token]  -> Either (Int, ParserError) Program
parser' pos commands tokens = case parseCommand pos tokens of
  Right (command, [Semicolon]) -> Right (commands ++ [command])
  Right (command, Semicolon:rest) ->
    parser' (pos + length tokens - length rest) (commands ++ [command]) rest
  Left err -> Left err
  Right what -> Left (pos, ParserError $ "parseCommand returned" ++ show what)
  -- _ -> Left (pos, ParserError "parser' error")

-- command ::= assignment | functionCall | patternMatching
parseCommand :: Subparser
parseCommand pos [] = Right ((Empty, pos), [])
parseCommand pos tokens = case (parseErrors, parseGood) of
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
        parseResultsEither = map (\f -> f pos tokens) subparsers
        (parseErrors, parseGood) = partitionEithers parseResultsEither
        furthestError = maximumBy (compare `on` fst) parseErrors
        furthestGood = minimumBy (compare `on` (length . snd)) parseGood
        furthestGoodDist = ((-) `on` length) tokens (snd furthestGood) + pos
        furthest = if furthestGoodDist > fst furthestError
          then Right furthestGood
          else Left furthestError


-- assignment ::= id "<-" expr
parseAssignment :: Subparser
parseAssignment pos (Id string:AssignmentOperator:rest) =
  case parseExpr (pos + 2) rest of
    Right (rhs, rest') ->
      Right ((Assignment (ExprId string, pos) rhs, pos), rest')
    Left err -> Left err
parseAssignment pos _ = Left (pos, ParserError "parseAssignment error")

-- functionCall ::= id '(' {expr} ')'
parseFunctionCall :: Subparser
parseFunctionCall pos [] = Left (pos, ParserError "parseExpr empty")
parseFunctionCall pos (Id strId:ParenthesisLeft:rest) =
  case parseFunctionArgs pos rest of
    Right (args, rest') ->
      Right ((FunctionCall (ExprId strId, pos) args, pos), rest')
    Left err -> Left err
parseFunctionCall pos tokens = Left (pos, ParserError "parseFunctionCall error")

parseFunctionArgs :: Int -> [Token]
                  -> Either (Int, ParserError) ([ExprPos], [Token])
parseFunctionArgs = parseFunctionArgs' []

parseFunctionArgs' :: [ExprPos] -> Int -> [Token]
                   -> Either (Int, ParserError) ([ExprPos], [Token])
parseFunctionArgs' args pos (ParenthesisRight:rest) = Right (args, rest)
parseFunctionArgs' args pos tokens = case parseExpr pos tokens of
  Right (arg, rest) ->
    parseFunctionArgs' (args ++ [arg]) (pos + length tokens - length rest) rest
  Left err -> Left err

-- patternMatching ::= 'match' expr '{'
-- 	{expr '->' expr ';'}
-- 	'_' '->' expr ';' '}'
parsePatternMatching :: Subparser
parsePatternMatching pos (MatchKeyword:rest) = case parseExpr (pos + 1) rest of
  Left err -> Left err
  Right (switch, BraceLeft:rest') -> case parseMatchBody (pos + 1 + length rest - length rest') rest' of
    Right (cases, defaultCase, rest'') ->
      Right ((PatternMatching switch cases defaultCase, pos), rest'')
    Left err -> Left err
parsePatternMatching pos _ = Left (pos, ParserError "parsePatternMatching error")

parseMatchBody :: Int -> [Token]
               -> Either (Int, ParserError) ([(ExprPos, ExprPos)], ExprPos, [Token])
parseMatchBody = parseMatchBody' []

parseMatchBody' cases pos (WildCard:MatchArrow:tokens) = case parseExpr (pos + 2) tokens of
  Right (defaultCase, Semicolon:BraceRight:rest) ->
    Right (cases, defaultCase, rest)
  Left err -> Left err
  _ -> Left (pos, ParserError "parseMatchBody' error default")
parseMatchBody' cases pos tokens = do
  (lhs, rest1) <- parseExpr pos tokens -- expr
  rest2 <- skipToken MatchArrow (posFromRest rest1) rest1 -- '->'
  (rhs, rest3) <- parseExpr (posFromRest rest2) rest2 -- expr
  rest4 <- skipToken Semicolon (posFromRest rest3) rest3 -- ';'
  parseMatchBody' (cases ++ [(lhs, rhs)]) (posFromRest rest4) rest4
  where posFromRest rest = pos + length tokens - length rest

skipToken :: Token -> Int -> [Token] -> Either (Int, ParserError) [Token]
skipToken token pos tokens = case listToMaybe tokens of
  Just first -> if first == token
    then Right $ tail tokens
    else Left (pos, error $ "`" ++ showConstr first ++ "`")
  Nothing -> Left (pos, error "empty list of tokens")
  where error suffix = ParserError $ "expected `" ++ showConstr token
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
parseExpr pos [] = Left (pos, ParserError "parseExpr empty")
parseExpr pos tokens = case (parseErrors, parseGood) of
  -- only errors from complex expressions ->
  -- if error is on first token we might still have singleTokenExpr
  (_ , []) -> case singleTokenExpr of
    Just expr | fst furthestError == pos -> Right ((expr, pos), tail tokens)
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
        parseResultsEither = map (\f -> f pos tokens) subparsers
        (parseErrors, parseGood) = partitionEithers parseResultsEither
        furthestError = maximumBy (compare `on` fst) parseErrors
        furthestGood = minimumBy (compare `on` (length . snd)) parseGood
        furthestGoodDist = ((-) `on` length) tokens (snd furthestGood) + pos
        furthest = if furthestGoodDist > fst furthestError
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
parseNamedTupleAcess pos (Id lhs:NamedTuppleAccessOperator:Id rhs:rest) =
  Right ((NamedTuppleAccess (ExprId lhs, pos) (ExprId rhs, pos + 2), pos), rest)
parseNamedTupleAcess pos _ =
  Left (pos, ParserError "parseNamedTupleAcess error")

-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
parseLambdaDef :: Subparser
parseLambdaDef pos tokens = do
  rest <- skipToken ParenthesisLeft pos tokens -- '('
  (args, rest1) <- parseLambdaArgs (pos + 1) rest -- {id} ')'
  rest2 <- skipToken BraceLeft (pos + length tokens - length rest1) rest1 -- '{'
  (body, rest3) <- parseLambdaBody (pos + length tokens - length rest2) rest2
  return ((LambdaDef args body, pos), rest3)

parseLambdaArgs :: Int -> [Token]
                -> Either (Int, ParserError) ([ExprPos], [Token])
parseLambdaArgs = parseLambdaArgs' []

parseLambdaArgs' :: [ExprPos] -> Int -> [Token]
                -> Either (Int, ParserError) ([ExprPos], [Token])
parseLambdaArgs' args _ (ParenthesisRight:rest) = Right (args, rest)
parseLambdaArgs' args pos (Id arg:rest) =
  parseLambdaArgs' (args ++ [(ExprId arg, pos)]) (pos + 1) rest
parseLambdaArgs' _ pos _ = Left (pos, ParserError "parseLambdaArgs' error")

parseLambdaBody :: Int -> [Token]
                -> Either (Int, ParserError) ([ExprPos], [Token])
parseLambdaBody = parseLambdaBody' []

parseLambdaBody' :: [ExprPos] -> Int -> [Token]
                 -> Either (Int, ParserError) ([ExprPos], [Token])
parseLambdaBody' commands _ (BraceRight:rest) = Right (commands, rest)
parseLambdaBody' commands pos tokens = case parseCommand pos tokens of
  Right (command, Semicolon:rest) ->
    parseLambdaBody' (commands ++ [command]) (pos + length tokens - length rest) rest
  Left err -> Left err
  Right r -> Left (pos, ParserError $ "parserLambdaBody' error, parseCommand returned: " ++ show r)
  -- _ -> Left (pos, ParserError "parseLambdaBody' error")

-- namedTuple ::= '[' {namedTupleField} ']'
-- namedTupleField ::= id '=' expr
parseNamedTuple :: Subparser
parseNamedTuple pos (BracketLeft:rest) = case parseNamedTupleFields pos rest of
  Right (fields, rest') -> Right ((NamedTuple fields, pos), rest')
  Left err -> Left err
parseNamedTuple pos tokens = Left (pos, ParserError "parseNamedTuple error")

parseNamedTupleFields :: Int -> [Token]
                 -> Either (Int, ParserError) ([(ExprPos, ExprPos)], [Token])
parseNamedTupleFields = parseNamedTupleFields' []

parseNamedTupleFields' :: [(ExprPos, ExprPos)] -> Int -> [Token]
                  -> Either (Int, ParserError) ([(ExprPos, ExprPos)], [Token])
parseNamedTupleFields' fields pos (BracketRight:rest) = Right (fields, rest)
parseNamedTupleFields' fields pos tokens = case parseNamedTupleField pos tokens of
  Right (lhs, rhs, rest) -> parseNamedTupleFields'
    (fields ++ [(lhs, rhs)]) (pos + length tokens - length rest) rest
  Left err -> Left err

parseNamedTupleField :: Int -> [Token]
                     -> Either (Int, ParserError) (ExprPos, ExprPos, [Token])
parseNamedTupleField pos (Id lhs:NamedTuppleBindingOperator:rest) =
  case parseExpr (pos + 2) rest of
    Right (rhs, rest') -> Right ((ExprId lhs, pos), rhs, rest')
    Left err -> Left err
parseNamedTupleField pos _ = Left (pos, ParserError "parseNamedTupleField error")

-- tuple ::= '[' {expr} ']'
parseTuple :: Subparser
parseTuple pos (BracketLeft:rest) = case parseTupleFields pos rest of
  Right (fields, rest') -> Right ((Tuple fields, pos), rest')
  Left err -> Left err
parseTuple pos tokens = Left (pos, ParserError $ "parseTuple error, tokens: " ++ show tokens)

parseTupleFields :: Int -> [Token]
                 -> Either (Int, ParserError) ([ExprPos], [Token])
parseTupleFields = parseTupleFields' []

parseTupleFields' :: [ExprPos] -> Int -> [Token]
                  -> Either (Int, ParserError) ([ExprPos], [Token])
parseTupleFields' fields pos (BracketRight:rest) = Right (fields, rest)
parseTupleFields' fields pos tokens = case parseExpr pos tokens of
  Right (field, rest) -> parseTupleFields'
    (fields ++ [field]) (pos + length tokens - length rest) rest
  Left err -> Left err