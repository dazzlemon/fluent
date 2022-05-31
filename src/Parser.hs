import Lexer

-- program ::= { command ';'}
newtype Program = Program [Expr]

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

data ParserError = ParserError
--               tokens  -> Either (unexpectedTokenOffset, err) (subAST, rest)
type Subparser = [Token] -> Either (Int,           ParserError) (Expr, [Token])
 

parser :: [Token] -> Either ParserError Program
parser tokens = parser' tokens []

parser' :: [Token] -> Program -> Either ParserError Program
parser' tokens commands = case parseCommand of
	Right (command, []) -> commands ++ command
	Right (command, rest) -> parser' tokens (commands ++ command)
	_ -> Left ParserError

-- command ::= assignment | functionCall | patternMatching
parseCommand :: [Token] -> Either ParserError (Expr, [Token])
parseCommand tokens = Left ParserError -- TODO: unimplemented
	where parsedAssignment = parseAssignment tokens
	      parsedFunctionCall = parseFunctionCall tokens
	      parsedPatternMatching = parsePatternMatching tokens

-- assignment ::= id "<-" expr
parseAssignment :: [Token] -> Either ParserError (Expr, [Token])

-- functionCall ::= id '(' {expr} ')'
parseFunctionCall :: Subparser

-- patternMatching ::= 'match' expr '{'
-- 	{expr '->' expr ';'}
-- 	'_' '->' expr ';' '}'
parsePatternMatching :: [Token] -> Either ParserError (Expr, [Token])

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
parseExpr [] = Right (Empty, [])
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

-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
parseLambdaDef :: Subparser

-- namedTuple ::= '[' {namedTupleField} ']'
-- namedTupleAcess ::= id ':' id
parseNamedTuple :: Subparser

-- tuple ::= '[' {expr} ']'
parseTuple :: Subparser