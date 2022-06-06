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
evaluator' variableScopes (first:rest) = case first of
  Assignment (ExprId lhs) rhs -> case evalExpr variableScopes rhs of
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
          -- maybeVariableScopes = do
          --   -- find first (deepest possible) variableScope
          --   -- that contains var to be assigned to
          --   i <- findIndex (containsVar lhs) variableScopes
          --   let ith = variableScopes !! i
          --   -- if there is a scope that has var assigned to
          --   -- find index of the var in the scope
          --   j <- findIndex ((== lhs) . fst) ith
          --   -- replace j-th variable value with rhs
          --   let ith' = replaceNth j newKV ith
          --   -- replace i-th scope with modified one:
          --   return $ replaceNth i ith' variableScopes
          -- variableScopes' = case maybeVariableScopes of
          --   Just r -> r
          --   Nothing -> (newKV:head variableScopes):tail variableScopes
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

evalExpr :: [[(String, Variable)]] -> Expr -> (Maybe Variable, IO ())
evalExpr _ (ExprNumber str) = (Just $ VarNumber str, return ())
evalExpr _ (ExprString str) = (Just $ VarString str, return ())
evalExpr variableScopes (ExprId str) = (findVarById str variableScopes, return ())
evalExpr _ (LambdaDef argNames commandList) = (Just $ VarFunction argNames' commandList, return ())
  where argNames' = map str argNames
evalExpr variableScopes (FunctionCall (ExprId fname) args) = 
  case findVarById fname variableScopes of
    Just (VarFunction argNames body) -> if length argNames /= length args
      then (Nothing, expectedArgs fname (length argNames) (length args))
      else case evaluator' (variableScopesWith argNames) (init body) of
        (varScopes', io1) -> case evalExpr varScopes' (last body) of
          (res, io2) -> (res, io1 >> io2)
    Just _ -> (Nothing, die $ "error: `" ++ fname ++ "` is not a function")
    Nothing -> case fname of
      "print" -> if length args /= 1
        then (Nothing, expectedArgs "print" 1 $ length args)
        else case head args of
          ExprString str -> (Nothing, putStrLn str)
          ExprNumber str -> (Nothing, putStrLn str)
          ExprId str -> case findVarById str variableScopes of
            Just var -> (Nothing, printVar var)
          fc@(FunctionCall _ _) -> case evalExpr variableScopes fc of
            (Just arg, io) -> (Nothing, io >> printVar arg)
            (Nothing, io) -> (Nothing, io)
            -- (Nothing, io) -> (Nothing, io >> die "error: argument expression wasn't evaluated")
          _ -> (Nothing, die "not implemented yet") -- TODO:
      _ | fname `elem` ["add", "sub"] -> binaryNumFun
      _ -> (Nothing, die $ "error: `" ++ fname ++ "` is not initialized, varScopes: " ++ show variableScopes)
  where mbVar = evalExpr variableScopes 
        argVars = catMaybes argMaybeVars
        (argMaybeVars, ios) = unzip evalExprs
        evalExprs = map (evalExpr variableScopes) args
        variableScopesWith argNames = zip argNames argVars:variableScopes
        binaryNumFun = if argCount /= 2
            then err
            else mergeTwoExprs f1 f2 variableScopes a1 a2
          where a1 = head args
                a2 = args !! 1
                argCount = length args
                err = (Nothing, expectedArgs fname 2 argCount)
                (f1, f2) = case fname of
                  "add" -> ((+), (+))
                  "sub" -> ((-), (-))
        printVar var = case var of
          VarString str -> putStrLn str
          VarNumber str -> putStrLn str

evalExpr variableScopes (PatternMatching switch cases defaultCase) = case evalExpr variableScopes expr of
  (res, io2) -> (res, io >> io2)
  where (lefts, rights) = unzip cases
        (io, maybeIndex) = findMatch variableScopes switch lefts
        expr = case maybeIndex of
          Just i -> rights !! i
          Nothing -> defaultCase
evalExpr _ NullExpr = (Just VarNull, return ()) 
evalExpr _ e = (Nothing, die $ "error: this expression can't be evaluated: " ++ show e)

type BinaryFun a = (a -> a -> a)

-- first two args are the same function (but for Int and Double)
-- second is VarScopes to evaluate args for the function
-- third and fourth are args
--   that will merged into new value using first two functions
-- returns result of merging and IO that contains messages,
--   and may end with exitFailure
mergeTwoExprs :: BinaryFun Int -> BinaryFun Double
              -> VarScopes
              -> Expr -> Expr
              -> (Maybe Variable, IO ())
mergeTwoExprs f1 f2 varScopes lhs rhs = (res, io)
  where (res, (_, io)) = runState binExprs state_
        state_ = (varScopes, return ())
        binExprs = runMaybeT $ do
          lhsStr <- evalNum lhs
          rhsStr <- evalNum rhs
          return $ VarNumber $ if show (read lhsStr::Int) == lhsStr
            then show $ f1 (read lhsStr) (read rhsStr) 
            else show $ f2 (read lhsStr) (read rhsStr)

evalNum :: Expr -> MaybeT (State EvalState) String 
evalNum expr = MaybeT $ do
  (varScopes, io) <- get
  case evalExpr varScopes expr of
    (res, io') -> do
      put (varScopes, io >> io')
      case res of
        Just (VarNumber numStr) -> return (Just numStr)
        Nothing -> err "error: can't evaluate expr"
        _ -> err "expected number argument"
    where err msg = do
            (varScopes, io) <- get
            -- put (varScopes, io >> die msg)
            put (varScopes, io)
            return Nothing
