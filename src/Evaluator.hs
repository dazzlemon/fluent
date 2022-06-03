module Evaluator where

import Parser (Program, Expr)

evaluator :: Program -> IO ()

data Variable = VarNumber { varStr::String }
              | VarString { varStr::String }
              -- function will be evaluated every time with new args
              | VarFunction { argNames::[String], body::Program }
              | VarTuple { fields::[Variable] }
              | VarNamedTuple { namedFields::[(String, Variable)] }
							| VarNull
							| VarWildCard

evalExpr :: [[(String, Variable)]] -> Expr -> IO ()
-- evalExpr variableScopes expr =
	-- ExprNumber str -- VarNumber str
	-- ExprString str -- VarString str
	-- ExprId str -- get value from deepest variableScope
	-- FunctionCall id args
		-- get value from deepest variableScope
		-- set args in new variableScope
		-- call evalExpr for body of the function
	-- WildCardExpr -- VarWildCard
	-- NullExpr -- VarNull
	-- NamedTuppleAccess tupleName fieldName
		-- get value from deepest variableScope
		-- get field
	-- LambdaDef argNames commandList -- VarFunction
	-- Assignment lhs rhs
		-- if it doesn't exist in current variableScope
			-- then: add it to current variableScope
			-- else: assign new value
	-- PatternMatching switch cases defaultCase
		-- try to match switch against cases, if it doesnt match -> default case
		-- result = rhs expr of first matching case (or default)
	-- Tuple values -- VarTuple
	-- NamedTuple -- VarNamedTuple
	-- Empty -- TODO: remove
