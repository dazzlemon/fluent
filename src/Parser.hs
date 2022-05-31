{-# LANGUAGE DeriveDataTypeable #-}
module Parser where

import Lexer
import Data.List (partition, maximumBy, minimumBy)
import Data.Either (isLeft)
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

data ParserError = ParserError
--               tokens  -> Either (unexpectedTokenOffset, err) (subAST, rest)
type Subparser = [Token] -> Either (Int,           ParserError) (Expr, [Token])
 
parser :: [Token] -> Either ParserError Program
parser tokens = parser' tokens []

-- program ::= { command ';'}
parser' :: [Token] -> Program -> Either ParserError Program
parser' tokens commands = case parseCommand tokens of
	Right (command, []) -> Right (commands ++ [command])
	Right (command, Semicolon:rest) -> parser' rest (commands ++ [command])
	_ -> Left ParserError

-- command ::= assignment | functionCall | patternMatching
parseCommand :: Subparser
parseCommand [] = Right (Empty, [])
parseCommand tokens = case (parseErrors, parseGood) of
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
	      parseResultsEither = map ($ tokens) subparsers
	      (parseResultsLeft, parseResultsRight) =
					partition isLeft parseResultsEither
	      parseErrors = map (\(Left l) -> l) parseResultsLeft
	      parseGood   = map (\(Right r) -> r) parseResultsLeft
	      furthestError = maximumBy (compare `on` fst) parseErrors
	      furthestGood = minimumBy (compare `on` (length . snd)) parseGood
	      furthestGoodDist = ((-) `on` length) tokens $ snd furthestGood
	      furthest = if furthestGoodDist > fst furthestError
	        then Right furthestGood
	        else Left furthestError


-- assignment ::= id "<-" expr
parseAssignment :: Subparser
parseAssignment (Id string:AssignmentOperator:rest) = case parseExpr rest of
	Right (expr, rest') -> Right (Assignment (ExprId string) expr, rest')
	Left err -> Left err

-- functionCall ::= id '(' {expr} ')'
parseFunctionCall :: Subparser
parseFunctionCall _ = Left (0, ParserError) -- TODO: unimplemented

-- patternMatching ::= 'match' expr '{'
-- 	{expr '->' expr ';'}
-- 	'_' '->' expr ';' '}'
parsePatternMatching :: Subparser
parsePatternMatching _ = Left (0, ParserError) -- TODO: unimplemented

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
parseExpr [] = Left (0, ParserError)
parseExpr tokens = case (parseErrors, parseGood) of
	-- only errors from complex expressions ->
	-- if error is on first token we might still have singleTokenExpr
	(_ , []) -> case singleTokenExpr of
		Just expr | fst furthestError == 1 -> Right (expr, tail tokens)
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
	      parseResultsEither = map ($ tokens) subparsers
	      (parseResultsLeft, parseResultsRight) =
					partition isLeft parseResultsEither
	      parseErrors = map (\(Left l) -> l) parseResultsLeft
	      parseGood   = map (\(Right r) -> r) parseResultsLeft
	      furthestError = maximumBy (compare `on` fst) parseErrors
	      furthestGood = minimumBy (compare `on` (length . snd)) parseGood
	      furthestGoodDist = ((-) `on` length) tokens $ snd furthestGood
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
parseNamedTupleAcess _ = Left (0, ParserError) -- TODO: unimplemented

-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
parseLambdaDef :: Subparser
parseLambdaDef _ = Left (0, ParserError) -- TODO: unimplemented

-- namedTuple ::= '[' {namedTupleField} ']'
-- namedTupleAcess ::= id ':' id
parseNamedTuple :: Subparser
parseNamedTuple _ = Left (0, ParserError) -- TODO: unimplemented

-- tuple ::= '[' {expr} ']'
parseTuple :: Subparser
parseTuple _ = Left (0, ParserError) -- TODO: unimplemented