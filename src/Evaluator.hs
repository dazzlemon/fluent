{-# LANGUAGE FlexibleContexts #-}

module Evaluator where

import System.Exit (exitFailure)
import Parser
import Control.Monad (mplus)
import Control.Monad.State
import Data.List (findIndex, elemIndex)
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.Trans.Except ( ExceptT
                                  , except
                                  , throwE
                                  , runExceptT
                                  , withExceptT
                                  , catchE
                                  )
import Stuff

data EvaluatorError = InitialError { pos::Int, what::String }
                    | StackTrace   { pos::Int, what::String
                                   , trace::EvaluatorError }
                    deriving (Show)

evaluator :: Program -> IO (Maybe EvaluatorError)
evaluator p = do
  res <- runExceptT $ evaluator' [[]] p
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

evaluator' :: VarScopes -> Program -> ExceptT EvaluatorError IO VarScopes
evaluator' variableScopes [] = return variableScopes
evaluator' variableScopes ((first, pos):rest) = case first of
    Assignment (ExprId lhs, pos1) rhs -> do
      rhsVar <- evalExpr variableScopes rhs
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
      evaluator' variableScopes' rest
    FunctionCall {} -> evalExpr'
    PatternMatching {} -> evalExpr'
    NullExpr -> evaluator' variableScopes rest -- just skip it
    other -> throwInitial pos $ "invalid comand: " ++ show other
  where evalExpr' = do
          _ <- evalExpr variableScopes (first, pos)
          evaluator' variableScopes rest

findMatch :: [[(String, Variable)]] -> ExprPos -> [ExprPos]
          -> ExceptT EvaluatorError IO (Maybe Int)
findMatch = findMatch' 0

findMatch' :: Int -> [[(String, Variable)]] -> ExprPos -> [ExprPos]
           -> ExceptT EvaluatorError IO (Maybe Int)
findMatch' _ _ _ [] = return Nothing
findMatch' i variableScopes lhs (rhs:rest) = do
  lhs' <- evalExpr variableScopes lhs
  rhs' <- evalExpr variableScopes rhs
  if match lhs' rhs'
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

type InnerFunction = VarScopes -> [ExprPos]
                  -> ExceptT EvaluatorError IO Variable

-- you are expected to check size of arguments
-- before passing them to inner function
innerFunctions :: [(String, (Int, InnerFunction))]
innerFunctions = [ ("print", (1, printExpr))
                 , ("add", (2, binaryNumFun "add" (+)))
                 , ("sub", (2, binaryNumFun "sub" (-)))
                 , ("read", (0, readExpr))
                 ]

readExpr :: InnerFunction
readExpr _ [] = VarString <$> liftIO getLine

binaryNumFun :: String -> (Double -> Double -> Double) -> InnerFunction
binaryNumFun fname f varScopes [a1, a2] = mergeTwoExprs fname f varScopes a1 a2

printExpr :: InnerFunction
printExpr varScopes [arg] = do
  var <- evalExpr varScopes arg
  case var of
    VarString str -> printStr str
    VarNumber str -> printStr str
    _ -> throwInitial (snd arg) "can only print numbers and strings"
  where printStr str = do
          liftIO $ putStrLn str
          return VarNull

evalExpr :: VarScopes -> ExprPos -> ExceptT EvaluatorError IO Variable
evalExpr _ (ExprNumber str, _) = return $ VarNumber str
evalExpr _ (ExprString str, _) = return $ VarString str
evalExpr variableScopes (ExprId str, pos) =
  case findVarById str variableScopes of
    Just var -> return var
    Nothing -> throwInitial pos $ "`" ++ str ++ "` is not initialized" 
evalExpr _ (LambdaDef argNames commandList, pos) =
  return $ VarFunction argNames' commandList
  where argNames' = map (str . fst) argNames
evalExpr variableScopes (FunctionCall (ExprId fname, _) args, pos) = 
  case findVarById fname variableScopes of
    Just (VarFunction argNames body) -> if length argNames /= length args
      then expectedNArgs (length argNames) (length args)
      else do
        argVars <- mapM (evalExpr variableScopes) args
        let variableScopes' = zip argNames argVars:variableScopes
        let stackTrace = withExceptT (StackTrace pos fname)
        varScopes' <- stackTrace $ evaluator' variableScopes' (init body)
        stackTrace $ evalExpr varScopes' (last body)
    Just _ -> throwInitial pos $ "`" ++ fname ++ "` is not a function"
    Nothing -> case lookup fname innerFunctions of
      Just (argSize, f) -> if argSize /= length args 
        then expectedNArgs argSize (length args)
        else f variableScopes args
      _ -> throwInitial pos $ "`" ++ fname ++ "` is not initialized"
  where expectedNArgs n n' = throwInitial pos $ "`" ++ fname ++ "` expected "
          ++ show n ++ " argument(s), but got "
          ++ show n'
evalExpr variableScopes (PatternMatching switch cases defaultCase, pos) = do
  let (lefts, rights) = unzip cases
  maybeIndex <- findMatch variableScopes switch lefts
  evalExpr variableScopes $ case maybeIndex of
    Just i -> rights !! i
    Nothing -> defaultCase
evalExpr _ (NullExpr, _) = return VarNull 
evalExpr _ (e, pos) = throwInitial pos "expression can't be evaluted"

type BinaryFun a = (a -> a -> a)

mergeTwoExprs :: String -> BinaryFun Double
              -> VarScopes
              -> ExprPos -> ExprPos
              -> ExceptT EvaluatorError IO Variable
mergeTwoExprs fname f varScopes lhs rhs = do
  lhsStr <- evalNum' lhs
  rhsStr <- evalNum' rhs
  let res = f (read lhsStr) (read rhsStr)
  return $ VarNumber $ if '.' `elem` lhsStr -- if not integer
    then show res
    else show (floor res::Int) 
  where evalNum' n = evalNum fname n varScopes

evalNum :: String -> ExprPos -> VarScopes
        -> ExceptT EvaluatorError IO String
evalNum fname (expr, pos) varScopes = do
  var <- evalExpr varScopes (expr, pos)
  case var of
    VarNumber numStr -> return numStr
    _ -> throwInitial pos $ fname ++ ": expected number argument"

throwInitial pos str = throwE $ InitialError pos str