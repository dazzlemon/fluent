{-# LANGUAGE FlexibleContexts #-}

module Evaluator where

import System.Exit (die)
import Parser
import Control.Monad (mplus)
import Control.Monad.State
import Data.List (findIndex, elemIndex)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.Trans.Maybe

evaluator :: Program -> IO ()
evaluator p = do
  _ <- evaluator' [[]] p
  return ()

type VarScopes = [[(String, Variable)]]
type EvalState = IO VarScopes

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

evaluator' :: VarScopes -> Program -> EvalState
evaluator' variableScopes [] = return variableScopes
evaluator' variableScopes ((first, pos):rest) = case first of
  Assignment (ExprId lhs, pos1) (rhs, pos2) -> do
    evaluated <- evalExpr variableScopes rhs
    case evaluated of
      Just rhsVar ->
        let newKV = (lhs, rhsVar)
            currentScope = head variableScopes
            currentScope' =
              -- try findind variable in current skope
              case findIndex ((== lhs) . fst) currentScope of
                --   change it
                Just i -> replaceNth i newKV currentScope
                --   otherwise add it
                _ -> newKV:currentScope
            variableScopes' = currentScope':tail variableScopes
        in evaluator' variableScopes' rest
  FunctionCall {} -> evalExpr'
  PatternMatching {} -> evalExpr'
  NullExpr -> evaluator' variableScopes rest -- just skip it
  other -> do
    die $ "error: invalid comand: " ++ show other
    return variableScopes
  where evalExpr' = do
          _ <- evalExpr variableScopes first
          varScopes <- evaluator' variableScopes rest
          return variableScopes

expectedArgs :: String -> Int -> Int -> IO ()
expectedArgs fname expected got = die $ "error: `" ++ fname
  ++ "` expected " ++ show expected ++ " argument(s), but got "
  ++ show got

findMatch :: [[(String, Variable)]] -> Expr -> [Expr] -> IO (Maybe Int)
findMatch = findMatch' 0

findMatch' :: Int -> [[(String, Variable)]] -> Expr -> [Expr] -> IO (Maybe Int)
findMatch' _ _ _ [] = return Nothing
findMatch' i variableScopes lhs (rhs:rest) = do
  mbLhsVar <- evalExpr variableScopes lhs
  mbRhsVar <- evalExpr variableScopes rhs
  case (mbLhsVar, mbRhsVar) of
    (Nothing, _) -> do
      die "error: lhs didn't evaluate"
      return Nothing
    (_, Nothing) -> do 
      die "error: rhs didn't evaluate"
      return Nothing
    (Just lhs', Just rhs') -> if match lhs' rhs'
      then return $ Just i
      else findMatch' (i + 1) variableScopes lhs rest

match :: Variable -> Variable -> Bool
match (VarNumber lhs) (VarNumber rhs) = rhs == lhs
match (VarString lhs) (VarString rhs) = rhs == lhs
match VarWildCard _ = True
match _ VarWildCard = True
match VarNull VarNull = True
match _ _ = False -- TODO: others won't match for now

findVarById :: String -> [[(String, Variable)]] -> Maybe Variable
findVarById name variableScopes = do
  i <- findIndex (containsVar name) variableScopes
  j <- findIndex ((== name) . fst) (variableScopes !! i)
  return (snd $ variableScopes !! i !! j)

containsVar :: String -> [(String, Variable)] -> Bool
containsVar name = any ((== name) . fst)
        
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n - 1) newVal xs

type InnerFunction = VarScopes -> [ExprPos]
                  -> IO (Either EvaluatorError Variable)

-- you are expected to check size of arguments
-- before passing them to inner function
innerFunctions :: [(String, (Int, InnerFunction))]
innerFunctions = [ ("print", (1, printExpr))
                 , ("add", (2, binaryNumFun (+)))
                 , ("sub", (2, binaryNumFun (-)))
                 , ("read", (0, readExpr))
                 ]

readExpr :: InnerFunction
readExpr _ [] = Right . VarString <$> getLine

binaryNumFun :: (Double -> Double -> Double) -> InnerFunction
binaryNumFun f varScopes [a1, a2] = mergeTwoExprs f varScopes a1 a2

printExpr :: InnerFunction
printExpr varScopes [(arg, pos)] = do
  var <- evalExpr varScopes arg
  case var of
    Just (VarString str) -> putStrLn str >> return (Right VarNull)
    Just (VarNumber str) -> putStrLn str >> return (Right VarNull)
    _ -> return $ Left $ InitialError pos "can only print numbers and strings"

evalExpr :: VarScopes -> Expr -> IO (Maybe Variable)
evalExpr _ (ExprNumber str) = return $ Just $ VarNumber str
evalExpr _ (ExprString str) = return $ Just $ VarString str
evalExpr variableScopes (ExprId str) = return $ findVarById str variableScopes
evalExpr _ (LambdaDef argNames commandList) =
  return $ Just $ VarFunction argNames' commandList
  where argNames' = map (str . fst) argNames
evalExpr variableScopes (FunctionCall (ExprId fname, pos) args) = 
  case findVarById fname variableScopes of
    Just (VarFunction argNames body) -> if length argNames /= length args
      then do
        expectedArgs fname (length argNames) (length args)
        return Nothing
      else do
        let args' = map fst args
        let argsEval = map (evalExpr variableScopes) args'
        let argsEval' = sequence argsEval
        argVars <- argsEval'
        let argVars' = catMaybes argVars
        let variableScopes' = zip argNames argVars':variableScopes
        varScopes' <- evaluator' variableScopes' (init body)
        evalExpr varScopes' (fst (last body))
    Just _ -> do
      die $ "error: `" ++ fname ++ "` is not a function"
      return Nothing
    Nothing -> case lookup fname innerFunctions of
      Just (argSize, f) -> if argSize /= length args 
        then do
          expectedArgs fname 2 argSize
          return Nothing
        else do
          res <- f variableScopes args
          case res of
            Left e -> die $ what e -- TODO:
            Right r -> return $ Just r
      _ -> do
        die $ "error: `" ++ fname ++ "` is not initialized, varScopes: " ++ show variableScopes
        return Nothing

evalExpr variableScopes (PatternMatching (switch, _) cases defaultCase) = do
  maybeIndex <- findMatch variableScopes switch (map fst lefts)
  let expr = fst $ case maybeIndex of
        Just i -> rights !! i
        Nothing -> defaultCase
  evalExpr variableScopes expr
  where (lefts, rights) = unzip cases
evalExpr _ NullExpr = return $ Just VarNull 
evalExpr _ e = do
  die $ "error: this expression can't be evaluated: " ++ show e
  return Nothing

type BinaryFun a = (a -> a -> a)

mergeTwoExprs :: BinaryFun Double
              -> VarScopes
              -> ExprPos -> ExprPos
              -> IO (Either EvaluatorError Variable)
mergeTwoExprs f varScopes lhs rhs = do
  l <- evalNum lhs varScopes
  case l of
    Right lhsStr -> do
      r <- evalNum rhs varScopes
      case r of
        Right rhsStr -> let resStr = if '.' `elem` lhsStr
                              then show res
                              else show (floor res::Int) 
                            res = f (read lhsStr) (read rhsStr)
                        in return $ Right $ VarNumber resStr
        Left e -> return $ Left e
    Left e -> return $ Left e

data EvaluatorError = InitialError { pos::Int, what::String }
                    | StackTrace   { pos::Int, what::String
                                   , trace::EvaluatorError }

evalNum :: ExprPos -> VarScopes -> IO (Either EvaluatorError String)
evalNum (expr, pos) varScopes = do
  res <- evalExpr varScopes expr
  case res of
    Just (VarNumber numStr) -> return $ Right numStr
    Nothing -> return $ Left $ InitialError pos "can't evaluate expr"
    _ -> return $ Left $ InitialError pos "expected number argument"
