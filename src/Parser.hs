import Lexer

-- program ::= { command ';'}
newtype Program = Program [Command]

-- expr ::= number
data Expr = Number { str::String }
--        | string
          | String { str::String }
--        | id
					| Id { str::String }
--        | functionCall
	-- functionCall ::= id '(' {expr} ')'
					| FunctionCall { id::Expr, args::[Expr] }
--        | '_'
					| WildCard
--        | "NULL"
					| Null
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

data ParserError = ParserError

parser :: [Token] -> Either ParserError Program
parser tokens = parser' tokens []

parser' :: [Token] -> Program -> Either ParserError Program
parser' tokens commands = case parseCommand of
	Right (command, []) -> commands ++ command
	Right (command, rest) -> parser' tokens (commands ++ command)
	_ -> Left ParserError

-- command ::= assignment | functionCall | patternMatching
parseCommand :: [Token] -> Either ParserError (Expr, [Token])
parseCommand tokens = Left ParserError
	where parsedAssignment = parseAssignment tokens
	      parsedFunctionCall = parseFunctionCall tokens
	      parsedPatternMatching = parsePatternMatching tokens

-- assignment ::= id "<-" expr
parseAssignment :: [Token] -> Either ParserError (Expr, [Token])

-- functionCall ::= id '(' {expr} ')'
parseFunctionCall :: [Token] -> Either ParserError (Expr, [Token])

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
parseExpr :: [Token] -> Either ParserError (Expr, [Token])

-- namedTupleAcess ::= id ':' id
parseNamedTupleAcess :: [Token] -> Either ParserError (Expr, [Token])

-- lambdaDef ::= '(' {id} ')' 
-- 	'{' { command ';'} '}'
parseLambdaDef :: [Token] -> Either ParserError (Expr, [Token])

-- namedTuple ::= '[' {namedTupleField} ']'
-- namedTupleAcess ::= id ':' id
parseNamedTuple :: [Token] -> Either ParserError (Expr, [Token])

-- tuple ::= '[' {expr} ']'
parseTuple :: [Token] -> Either ParserError (Expr, [Token])