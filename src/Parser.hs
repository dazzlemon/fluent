{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Lexer
import Data.List (maximumBy, minimumBy)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Data (Typeable, Data)

-- program ::= { command ';'}
type Program = [Expr]

-- expr ::= number
data Expr = ExprNumber { str::String }
--        | string
          | ExprString { str::String }
--        | id
					| ExprId { str::String }
--        | functionCall
	-- functionCall ::= id '(' {expr} ')'
					| FunctionCall { id::Expr, args::[Expr] }
--        | '_'
					| WildCardExpr
--        | "NULL"
					| NullExpr
--        | namedTupleAcess
					| NamedTuppleAccess { tupleName::Expr, fieldName::Expr }
--        | lambdaDef
-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
					| LambdaDef { argNames::[Expr], commandList::[Expr] }
					| Assignment { lhs::Expr, rhs::Expr }
	-- patternMatching ::= 'match' expr '{'
	--   {expr '->' expr ';'}
	--   '_' '->' expr ';' '}'
					| PatternMatching { switch::Expr
														, cases::[(Expr, Expr)]
														, defaultCase::(Expr, Expr)
														}
-- tuple ::= '[' {expr} ']'
					| Tuple { values::[Expr] }
-- namedTupleField ::= id '=' expr
-- namedTuple ::= '[' {namedTupleField} ']'
					| NamedTuple { keyValuePairs::[(Expr, Expr)] }
					| Empty -- kinda like TokenEOF
					deriving (Show, Data, Eq)

newtype ParserError = ParserError String deriving (Show, Data, Eq)

--               pos    tokens
type Subparser = Int -> [Token]
--                      (unexpectedTokenOffset, err) (subAST, rest)
              -> Either (Int,           ParserError) (Expr, [Token])
 
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
parseCommand _ [] = Right (Empty, [])
parseCommand pos tokens = case (parseErrors, parseGood) of
	-- only errors -> furthest error
	(_ , []) -> Left furthestError
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
		Right (expr, rest') -> Right (Assignment (ExprId string) expr, rest')
		Left err -> Left err
parseAssignment pos _ = Left (pos, ParserError "parseAssignment error")

-- functionCall ::= id '(' {expr} ')'
parseFunctionCall :: Subparser
parseFunctionCall pos [] = Left (pos, ParserError "parseExpr empty")
parseFunctionCall pos (Id strId:ParenthesisLeft:rest) =
	case parseFunctionArgs pos rest of
		Right (args, rest') ->
			Right (FunctionCall (ExprId strId) args, rest')
		Left err -> Left err
parseFunctionCall pos tokens = Left (pos, ParserError "parseFunctionCall error")

parseFunctionArgs :: Int -> [Token]
                  -> Either (Int, ParserError) ([Expr], [Token])
parseFunctionArgs = parseFunctionArgs' []

parseFunctionArgs' :: [Expr] -> Int -> [Token]
                   -> Either (Int, ParserError) ([Expr], [Token])
parseFunctionArgs' args pos (ParenthesisRight:rest) = Right (args, rest)
parseFunctionArgs' args pos tokens = case parseExpr pos tokens of
	Right (arg, rest) ->
		parseFunctionArgs' (args ++ [arg]) (pos + length tokens - length rest) rest
	Left err -> Left err

-- patternMatching ::= 'match' expr '{'
-- 	{expr '->' expr ';'}
-- 	'_' '->' expr ';' '}'
parsePatternMatching :: Subparser
parsePatternMatching pos _ = Left (pos, ParserError "parsePatternMatching unimplemented") -- TODO: unimplemented

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
		Just expr | fst furthestError == pos -> Right (expr, tail tokens)
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
	Right (NamedTuppleAccess (ExprId lhs) (ExprId rhs), rest)
parseNamedTupleAcess pos _ =
	Left (pos, ParserError "parseNamedTupleAcess error")

-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
parseLambdaDef :: Subparser
parseLambdaDef pos (ParenthesisLeft:rest) = case parseLambdaArgs pos rest of
	Right (args, BraceLeft:rest') ->
		case parseLambdaBody (pos + length rest - length rest') rest' of
			Right (body, rest'') -> Right (LambdaDef args body, rest'')
			Left err -> Left err
	Left err -> Left err
	Right (_, rest') -> 
		-- Left (pos + length rest - length rest', ParserError "parseLambdaDef error body")
		Left (pos + length rest - length rest', ParserError $ "parseLambdaDef error body: " ++ show rest')
parseLambdaDef pos _ = Left (pos, ParserError "parseLambdaDef error")

parseLambdaArgs :: Int -> [Token]
                -> Either (Int, ParserError) ([Expr], [Token])
parseLambdaArgs = parseLambdaArgs' []

parseLambdaArgs' :: [Expr] -> Int -> [Token]
                -> Either (Int, ParserError) ([Expr], [Token])
parseLambdaArgs' args _ (ParenthesisRight:rest) = Right (args, rest)
parseLambdaArgs' args pos (Id arg:rest) =
	parseLambdaArgs' (args ++ [ExprId arg]) (pos + 1) rest
parseLambdaArgs' _ pos _ = Left (pos, ParserError "parseLambdaArgs' error")

parseLambdaBody :: Int -> [Token]
                -> Either (Int, ParserError) ([Expr], [Token])
parseLambdaBody = parseLambdaBody' []

parseLambdaBody' :: [Expr] -> Int -> [Token]
                 -> Either (Int, ParserError) ([Expr], [Token])
parseLambdaBody' commands _ (BraceRight:rest) = Right (commands, rest)
parseLambdaBody' commands pos tokens = case parseCommand pos tokens of
	Right (command, Semicolon:rest) ->
		parseLambdaBody' (commands ++ [command]) (pos + length tokens - length rest) rest
	Left err -> Left err
	_ -> Left (pos, ParserError "parseLambdaBody' error")

-- namedTuple ::= '[' {namedTupleField} ']'
-- namedTupleField ::= id '=' expr
parseNamedTuple :: Subparser
parseNamedTuple pos (BracketLeft:rest) = case parseNamedTupleFields pos rest of
	Right (fields, rest') -> Right (NamedTuple fields, rest')
	Left err -> Left err
parseNamedTuple pos tokens = Left (pos, ParserError "parseNamedTuple error")

parseNamedTupleFields :: Int -> [Token]
                 -> Either (Int, ParserError) ([(Expr, Expr)], [Token])
parseNamedTupleFields = parseNamedTupleFields' []

parseNamedTupleFields' :: [(Expr, Expr)] -> Int -> [Token]
                  -> Either (Int, ParserError) ([(Expr, Expr)], [Token])
parseNamedTupleFields' fields pos (BracketRight:rest) = Right (fields, rest)
parseNamedTupleFields' fields pos tokens = case parseNamedTupleField pos tokens of
	Right (lhs, rhs, rest) -> parseNamedTupleFields'
		(fields ++ [(lhs, rhs)]) (pos + length tokens - length rest) rest
	Left err -> Left err

parseNamedTupleField :: Int -> [Token]
                     -> Either (Int, ParserError) (Expr, Expr, [Token])
parseNamedTupleField pos (Id lhs:NamedTuppleBindingOperator:rest) =
	case parseExpr (pos + 2) rest of
		Right (rhs, rest') -> Right (ExprId lhs, rhs, rest')
		Left err -> Left err
parseNamedTupleField pos _ = Left (pos, ParserError "parseNamedTupleField error")

-- tuple ::= '[' {expr} ']'
parseTuple :: Subparser
parseTuple pos (BracketLeft:rest) = case parseTupleFields pos rest of
	Right (fields, rest') -> Right (Tuple fields, rest')
	Left err -> Left err
parseTuple pos tokens = Left (pos, ParserError "parseTuple error")

parseTupleFields :: Int -> [Token]
                 -> Either (Int, ParserError) ([Expr], [Token])
parseTupleFields = parseTupleFields' []

parseTupleFields' :: [Expr] -> Int -> [Token]
                  -> Either (Int, ParserError) ([Expr], [Token])
parseTupleFields' fields pos (BracketRight:rest) = Right (fields, rest)
parseTupleFields' fields pos tokens = case parseExpr pos tokens of
	Right (field, rest) -> parseTupleFields'
		(fields ++ [field]) (pos + length tokens - length rest) rest
	Left err -> Left err