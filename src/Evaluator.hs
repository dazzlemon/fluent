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
evaluator p = snd $ evaluator' [[]] p

type VarScopes = [[(String, Variable)]]
type EvalState = (VarScopes, IO ())

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
evaluator' variableScopes [] = (variableScopes, return ())
evaluator' variableScopes ((first, pos):rest) = case first of
  Assignment (ExprId lhs, pos1) (rhs, pos2) -> case evalExpr variableScopes rhs of
    (Just rhsVar, io) ->
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
      in case evaluator' variableScopes' rest of
        (vs, io') -> (vs, io >> io')
  FunctionCall {} -> evalExpr'
  PatternMatching {} -> evalExpr'
  NullExpr -> evaluator' variableScopes rest -- just skip it
  other -> (variableScopes, die $ "error: invalid comand: " ++ show other)
  where evalExpr' = case evalExpr variableScopes first of
          (_, io1) -> case evaluator' variableScopes rest of
            (varScopes, io2) -> (variableScopes, io1 >> io2)

expectedArgs :: String -> Int -> Int -> IO ()
expectedArgs fname expected got = die $ "error: `" ++ fname
  ++ "` expected " ++ show expected ++ " argument(s), but got "
  ++ show got

findMatch :: [[(String, Variable)]] -> Expr -> [Expr] -> (IO (), Maybe Int)
findMatch = findMatch' 0

findMatch' :: Int -> [[(String, Variable)]] -> Expr -> [Expr] -> (IO (), Maybe Int)
findMatch' _ _ _ [] = (return (), Nothing)
findMatch' i variableScopes lhs (rhs:rest) = (io, maybeVar)
  where io = io1 >> io2 >> io3
        (mbLhsVar, io1) = evalExpr variableScopes lhs
        (mbRhsVar, io2) = evalExpr variableScopes rhs
        (io3, maybeVar) = case (mbLhsVar, mbRhsVar) of
          (Nothing, _) -> ( die "error: lhs didn't evaluate"
                          , Nothing)
          (_, Nothing) -> ( die "error: rhs didn't evaluate"
                          , Nothing)
          (Just lhs', Just rhs') -> if match lhs' rhs'
            then (return (), Just i)
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

type InnerFunction = VarScopes -> [ExprPos] -> (Maybe Variable, IO ())

-- you are expected to check size of arguments
-- before passing them to inner function
innerFunctions :: [(String, (Int, InnerFunction))]
innerFunctions = [ ("print", (1, printExpr))
                 , ("add", (2, binaryNumFun (+)))
                 , ("sub", (2, binaryNumFun (-)))
                 ]

binaryNumFun :: (Double -> Double -> Double) -> InnerFunction
binaryNumFun f varScopes args = case mergeTwoExprs f varScopes a1 a2 of
    (Right res, io) -> (Just res, io)
    (Left e, io) -> (Nothing, io >> die (what e))
  where a1 = head args
        a2 = args !! 1
        argCount = length args

printExpr :: InnerFunction
printExpr varScopes args = if length args /= 1
  then (Nothing, expectedArgs "print" 1 $ length args)
  else case fst $ head args of
    ExprString str -> (Nothing, putStrLn str)
    ExprNumber str -> (Nothing, putStrLn str)
    ExprId str -> case findVarById str varScopes of
      Just var -> (Nothing, printVar var)
    fc@(FunctionCall _ _) -> case evalExpr varScopes fc of
      (Just arg, io) -> (Nothing, io >> printVar arg)
      (Nothing, io) -> (Nothing, io)
      -- (Nothing, io) -> (Nothing, io >> die "error: argument expression wasn't evaluated")
    _ -> (Nothing, die "not implemented yet") -- TODO:
  where printVar var = case var of
          VarString str -> putStrLn str
          VarNumber str -> putStrLn str

evalExpr :: VarScopes -> Expr -> (Maybe Variable, IO ())
evalExpr _ (ExprNumber str) = (Just $ VarNumber str, return ())
evalExpr _ (ExprString str) = (Just $ VarString str, return ())
evalExpr variableScopes (ExprId str) = (findVarById str variableScopes, return ())
evalExpr _ (LambdaDef argNames commandList) = (Just $ VarFunction argNames' commandList, return ())
  where argNames' = map (str . fst) argNames
evalExpr variableScopes (FunctionCall (ExprId fname, pos) args) = 
  case findVarById fname variableScopes of
    Just (VarFunction argNames body) -> if length argNames /= length args
      then (Nothing, expectedArgs fname (length argNames) (length args))
      else case evaluator' (variableScopesWith argNames) (init body) of
        (varScopes', io1) -> case evalExpr varScopes' (fst (last body)) of
          (res, io2) -> (res, io1 >> io2)
    Just _ -> (Nothing, die $ "error: `" ++ fname ++ "` is not a function")
    Nothing -> case lookup fname innerFunctions of
      Just (argSize, f) -> if argSize /= length args 
        then (Nothing, expectedArgs fname 2 argSize)
        else f variableScopes args
      _ -> (Nothing, die $ "error: `" ++ fname ++ "` is not initialized, varScopes: " ++ show variableScopes)
  where mbVar = evalExpr variableScopes 
        argVars = catMaybes argMaybeVars
        (argMaybeVars, ios) = unzip evalExprs
        evalExprs = map (evalExpr variableScopes . fst) args
        variableScopesWith argNames = zip argNames argVars:variableScopes

evalExpr variableScopes (PatternMatching (switch, _) cases defaultCase) = case evalExpr variableScopes expr of
  (res, io2) -> (res, io >> io2)
  where (lefts, rights) = unzip cases
        (io, maybeIndex) = findMatch variableScopes switch (map fst lefts)
        expr = fst $ case maybeIndex of
          Just i -> rights !! i
          Nothing -> defaultCase
evalExpr _ NullExpr = (Just VarNull, return ()) 
evalExpr _ e = (Nothing, die $ "error: this expression can't be evaluated: " ++ show e)

type BinaryFun a = (a -> a -> a)

mergeTwoExprs :: BinaryFun Double
              -> VarScopes
              -> ExprPos -> ExprPos
              -> (Either EvaluatorError Variable, IO ())
mergeTwoExprs f varScopes lhs rhs = case evalNum lhs varScopes of
  Right (lhsStr, io1) -> case evalNum rhs varScopes of
    Right (rhsStr, io2) -> let resStr = if '.' `elem` lhsStr
                                    then show res
                                    else show (floor res::Int) 
                               res = f (read lhsStr) (read rhsStr)
                           in (Right $ VarNumber resStr, io1 >> io2)
    Left e -> (Left e, io1)
  Left e -> (Left e, return ())

data EvaluatorError = InitialError { pos::Int, what::String }
                    | StackTrace   { pos::Int, what::String
                                   , trace::EvaluatorError }

evalNum :: ExprPos -> VarScopes -> Either EvaluatorError (String, IO ())
evalNum (expr, pos) varScopes = case evalExpr varScopes expr of
  (res, io') -> case res of
    Just (VarNumber numStr) -> Right (numStr, io')
    Nothing -> Left $ InitialError pos "can't evaluate expr"
    _ -> Left $ InitialError pos "expected number argument"
