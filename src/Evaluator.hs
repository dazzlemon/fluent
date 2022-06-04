module Evaluator where

import System.Exit (exitFailure)
import Parser
import Control.Monad (mplus)
import Data.List (findIndex, elemIndex)
import Data.Maybe (catMaybes, mapMaybe)

-- newtype EvaluatorError = EvaluatorError String

evaluator :: Program -> IO ()
evaluator = evaluator' [[]]

-- type InnerFunction = ([[(String, Variable)]] -> EvaluationResult)

data Variable = VarNumber { varStr::String }
              | VarString { varStr::String }
              -- function will be evaluated every time with new args
              | VarFunction { argNames::[String], body::Program }
              | VarTuple { fields::[Variable] }
              | VarNamedTuple { namedFields::[(String, Variable)] }
              | VarNull
              | VarWildCard
              -- | VarInnerFunction { argNames::[String]
              --                    , functionBody::InnerFunction
              --                    }
              deriving (Show)

evaluator' :: [[(String, Variable)]] -> Program -> IO ()
evaluator' _ [] = return ()
evaluator' variableScopes (first:rest) = case first of
  Assignment (ExprId lhs) rhs -> case evalExpr variableScopes rhs of
    (Just rhsVar, io) -> do
      io
      evalAfterAssignment lhs rhsVar variableScopes rest
  FunctionCall (ExprId "print") args -> if length args /= 1
    then expectedArgs "print" 1 $ length args
    else case head args of
      ExprString str -> printStr str
      ExprNumber str -> printStr str
      ExprId str -> printId str
      _ -> do
        putStrLn "not implemented yet"
        exitFailure -- TODO:
  FunctionCall (ExprId fname) args -> case findVarById fname variableScopes of
    Just (VarFunction argNames body) -> if length argNames /= length args
      then expectedArgs fname (length argNames) (length args)
      else do
        let evalExprs = map (evalExpr variableScopes) args
        let (argMaybeVars, ios) = unzip evalExprs
        let argVars = catMaybes argMaybeVars
        sequence_ ios
        evaluator' (zip argNames argVars:variableScopes) body
        evaluator' variableScopes rest
    Just _ -> do
      putStrLn $ "error: `" ++ fname ++ "` is not a function"
      exitFailure
    Nothing -> do
      putStrLn $ "error: `" ++ fname ++ "` is not initialized"
      exitFailure
  _ -> do
    putStrLn "not implemented yet"
    exitFailure -- TODO:
  where printStr str = do
          putStrLn str
          evaluator' variableScopes rest
        printId str = case findVarById str variableScopes of
          Just var -> case var of
            VarString val -> printStr val
            VarNumber val -> printStr val
            _ -> do
              putStrLn "not implemented yet"
              exitFailure -- TODO:
          _ -> do
            -- putStrLn $ "error: variable `" ++ str ++ "` not assigned"
            putStrLn $ "error: variable `" ++ str ++ "` not assigned, variableScopes: " ++ show variableScopes
            exitFailure -- TODO:
        expectedArgs fname expected got = do
          putStrLn $ "error: `" ++ fname
                  ++ "` expected " ++ show expected ++ " argument(s), but got "
                  ++ show got
          exitFailure

findVarById :: String -> [[(String, Variable)]] -> Maybe Variable
findVarById name variableScopes = do
  i <- findIndex (containsVar name) variableScopes
  j <- findIndex ((== name) . fst) (variableScopes !! i)
  return (snd $ variableScopes !! i !! j)

containsVar :: String -> [(String, Variable)] -> Bool
containsVar name = any $ (== name) . fst

evalAfterAssignment :: String -> Variable -> [[(String, Variable)]] -> Program -> IO ()
evalAfterAssignment lhs rhs variableScopes commands = do
  -- putStrLn $ "variableScopes: " ++ show variableScopes'
  evaluator' variableScopes' commands
  where variableScopes' = case maybeI of
          Just i -> case findIndex ((== lhs) . fst) (variableScopes !! i) of
            Just j -> replaceNth i (replaceNth j (lhs, rhs) (variableScopes !! i)) variableScopes
          Nothing -> ((lhs, rhs):head variableScopes):tail variableScopes
        maybeI = findIndex (containsVar lhs) variableScopes
        
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n - 1) newVal xs

evalExpr :: [[(String, Variable)]] -> Expr -> (Maybe Variable, IO ())
evalExpr _ (ExprNumber str) = (Just $ VarNumber str, return ())
evalExpr _ (ExprString str) = (Just $ VarString str, return ())
evalExpr variableScopes (ExprId str) = (findVarById str variableScopes, return ())
evalExpr _ (LambdaDef argNames commandList) = (Just $ VarFunction argNames' commandList, return ())
  where argNames' = map str argNames

-- evaluator :: Program -> IO ()
-- evaluator = mapM_ evalExpr

-- evalExpr :: Expr -> IO () 
-- evalExpr (FunctionCall (ExprId "print") [ExprString str]) = putStrLn str
-- evalExpr (FunctionCall (ExprId "print") [ExprNumber str]) = putStrLn str
-- evalExpr _ = do
--   putStrLn "not implemented yet"
--   exitFailure -- TODO:

-- type EvaluationResult = Either (Int, EvaluatorError) (Variable, IO ())

-- evaluator :: Program -> EvaluationResult
-- evaluator = evaluator' [] (IO ())

-- evaluator' :: [[(String, Variable)]] -> IO -> Program -> EvaluationResult
-- evaluator' _ io [] = Right (io) 

-- evalExpr :: [[(String, Variable)]] -> Expr -> EvaluationResult
-- -- evalExpr variableScopes expr =
-- 	-- ExprNumber str -- VarNumber str
-- 	-- ExprString str -- VarString str
-- 	-- ExprId str -- get value from deepest variableScope
-- 	-- FunctionCall id args
-- 		-- get value from deepest variableScope
-- 		-- set args in new variableScope
-- 		-- call evalExpr for body of the function
-- 	-- WildCardExpr -- VarWildCard
-- 	-- NullExpr -- VarNull
-- 	-- NamedTuppleAccess tupleName fieldName
-- 		-- get value from deepest variableScope
-- 		-- get field
-- 	-- LambdaDef argNames commandList -- VarFunction
-- 	-- Assignment lhs rhs
-- 		-- if it doesn't exist in current variableScope
-- 			-- then: add it to current variableScope
-- 			-- else: assign new value
-- 	-- PatternMatching switch cases defaultCase
-- 		-- try to match switch against cases, if it doesnt match -> default case
-- 		-- result = rhs expr of first matching case (or default)
-- 	-- Tuple values -- VarTuple
-- 	-- NamedTuple -- VarNamedTuple
-- 	-- Empty -- TODO: remove
