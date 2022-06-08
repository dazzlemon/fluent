{-# LANGUAGE FlexibleContexts #-}

module Evaluator where

import System.Exit (exitFailure)
import Parser
import Control.Monad (mplus)
import Control.Monad.State
import Data.List (findIndex, elemIndex)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.Trans.Maybe
import Data.Either (rights)

data EvaluatorError = InitialError { pos::Int, what::String }
                    | StackTrace   { pos::Int, what::String
                                   , trace::EvaluatorError }
                    deriving (Show)

evaluator :: Program -> IO (Maybe EvaluatorError)
evaluator p = do
  res <- evaluator' [[]] p
  case res of
    Right _ -> return Nothing
    Left e -> return $ Just e

type VarScopes = [[(String, Variable)]]

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

evaluator' :: VarScopes -> Program -> IO (Either EvaluatorError VarScopes)
evaluator' variableScopes [] = return $ Right variableScopes
evaluator' variableScopes ((first, pos):rest) = case first of
  Assignment (ExprId lhs, pos1) rhs -> do
    evaluated <- evalExpr variableScopes rhs
    case evaluated of
      Right rhsVar ->
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
      Left e -> return $ Left e 
  FunctionCall {} -> evalExpr'
  PatternMatching {} -> evalExpr'
  NullExpr -> evaluator' variableScopes rest -- just skip it
  other -> return $ Left $ InitialError pos $ "invalid comand: " ++ show other
  where evalExpr' = do
          res <- evalExpr variableScopes (first, pos)
          case res of
            Right _ -> evaluator' variableScopes rest
            Left e -> return $ Left e

findMatch :: [[(String, Variable)]] -> ExprPos -> [ExprPos]
          -> IO (Either EvaluatorError (Maybe Int))
findMatch = findMatch' 0

findMatch' :: Int -> [[(String, Variable)]] -> ExprPos -> [ExprPos]
           -> IO (Either EvaluatorError (Maybe Int))
findMatch' _ _ _ [] = return $ Right Nothing
findMatch' i variableScopes lhs (rhs:rest) = do
  mbLhsVar <- evalExpr variableScopes lhs
  mbRhsVar <- evalExpr variableScopes rhs
  case (mbLhsVar, mbRhsVar) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right lhs', Right rhs') -> if match lhs' rhs'
      then return $ Right $ Just i
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
printExpr varScopes [arg] = do
  r <- evalExpr varScopes arg
  case r of
    Right var -> case var of
      VarString str -> putStrLn str >> return (Right VarNull)
      VarNumber str -> putStrLn str >> return (Right VarNull)
      _ -> return $ Left $ InitialError (snd arg) "can only print numbers and strings"
    Left e -> return $ Left $ StackTrace (snd arg) "print" e

evalExpr :: VarScopes -> ExprPos -> IO (Either EvaluatorError Variable)
evalExpr _ (ExprNumber str, _) = return $ Right $ VarNumber str
evalExpr _ (ExprString str, _) = return $ Right $ VarString str
evalExpr variableScopes (ExprId str, pos) =
  return $ case findVarById str variableScopes of
    Just var -> Right var
    Nothing -> Left $ InitialError pos $ "`" ++ str ++ "` is not initialized" 
evalExpr _ (LambdaDef argNames commandList, pos) =
  return $ Right $ VarFunction argNames' commandList
  where argNames' = map (str . fst) argNames
evalExpr variableScopes (FunctionCall (ExprId fname, _) args, pos) = 
  case findVarById fname variableScopes of
    Just (VarFunction argNames body) -> if length argNames /= length args
      then return $ Left $ InitialError pos $ fname ++ "` expected "
        ++ show (length argNames) ++ " argument(s), but got "
        ++ show (length args)
      else do
        let argsEval = map (evalExpr variableScopes) args
        let argsEval' = sequence argsEval
        argVars <- argsEval'
        let argVars' = rights argVars
        let variableScopes' = zip argNames argVars':variableScopes
        ev <- evaluator' variableScopes' (init body)
        case ev of
          Left e -> return $ Left $ StackTrace pos fname e
          Right varScopes' -> evalExpr varScopes' (last body)
    Just _ -> 
      return $ Left $ InitialError pos $ "`" ++ fname ++ "` is not a function"
    Nothing -> case lookup fname innerFunctions of
      Just (argSize, f) -> if argSize /= length args 
        then return $ Left $ InitialError pos $ fname ++ "` expected "
                             ++ show 2 ++ " argument(s), but got "
                             ++ show argSize
        else f variableScopes args
      _ -> return $ Left $ InitialError pos $ "`" ++ fname ++ "` is not initialized"

evalExpr variableScopes (PatternMatching switch cases defaultCase, pos) = do
  maybeIndex <- findMatch variableScopes switch lefts
  let expr = case maybeIndex of
        Right (Just i) -> rights !! i
        Right Nothing -> defaultCase
  evalExpr variableScopes expr
  where (lefts, rights) = unzip cases
evalExpr _ (NullExpr, _) = return $ Right VarNull 
evalExpr _ (e, pos) =
  return $ Left $ InitialError pos "expression can't be evaluted"

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

evalNum :: ExprPos -> VarScopes -> IO (Either EvaluatorError String)
evalNum (expr, pos) varScopes = do
  res <- evalExpr varScopes (expr, pos)
  case res of
    Right (VarNumber numStr) -> return $ Right numStr
    Left e -> return $ Left e
    _ -> return $ Left $ InitialError pos "expected number argument"
