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
      fc@(FunctionCall _ _) -> case evalExpr variableScopes fc of
        (Just arg, io) -> do
          io
          case arg of
            VarString str -> printStr str
            VarNumber str -> printStr str
        (Nothing, io) -> do
          io
          exitWithErrorMessage "error: argument expression wasn't evaluated"
      _ -> exitWithErrorMessage "not implemented yet" -- TODO:
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
    Just _ -> exitWithErrorMessage $ "error: `" ++ fname ++ "` is not a function"
    Nothing -> exitWithErrorMessage $ "error: `" ++ fname ++ "` is not initialized"
  PatternMatching switch cases defaultCase -> do
    let (lefts, rights) = unzip cases
    -- TODO: if lhs is a function it will have to be evaluated, so one more io
    let (io, maybeIndex) = findMatch variableScopes switch lefts
    let expr = case maybeIndex of
                Just i -> rights !! i
                Nothing -> defaultCase
    io
    evaluator' variableScopes [expr]
    evaluator' variableScopes rest
  NullExpr -> evaluator' variableScopes rest -- just don't print it
  other -> exitWithErrorMessage $ "not implemented yet: " ++ show other -- TODO:
  where printStr str = do
          putStrLn str
          evaluator' variableScopes rest
        printId str = case findVarById str variableScopes of
          Just var -> case var of
            VarString val -> printStr val
            VarNumber val -> printStr val
            _ -> exitWithErrorMessage "not implemented yet" -- TODO:
          _ -> exitWithErrorMessage $ "error: variable `" ++ str ++ "` not assigned, variableScopes: " ++ show variableScopes -- TODO:

expectedArgs :: String -> Int -> Int -> IO ()
expectedArgs fname expected got = exitWithErrorMessage $ "error: `" ++ fname
  ++ "` expected " ++ show expected ++ " argument(s), but got "
  ++ show got

findMatch :: [[(String, Variable)]] -> Expr -> [Expr] -> (IO (), Maybe Int)
findMatch = findMatch' 0

findMatch' :: Int -> [[(String, Variable)]] -> Expr -> [Expr] -> (IO (), Maybe Int)
findMatch' _ _ _ [] = (return (), Nothing)
findMatch' i variableScopes lhs (rhs:rest) = (io, maybeVar)
  where io = do 
              io1
              io2
              io3
        (mbLhsVar, io1) = evalExpr variableScopes lhs
        (mbRhsVar, io2) = evalExpr variableScopes rhs
        (io3, maybeVar) = case (mbLhsVar, mbRhsVar) of
          (Nothing, _) -> ( exitWithErrorMessage "error: lhs didn't evaluate"
                          , Nothing)
          (_, Nothing) -> ( exitWithErrorMessage "error: rhs didn't evaluate"
                          , Nothing)
          (Just lhs', Just rhs') -> if match lhs' rhs'
            then (return (), Just i)
            else findMatch' (i + 1) variableScopes lhs rest

exitWithErrorMessage :: String -> IO ()
exitWithErrorMessage err = do
  putStrLn err
  exitFailure 

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
evalExpr variableScopes (FunctionCall (ExprId fname) args) = 
  case findVarById fname variableScopes of
    Just (VarFunction argNames body) -> case evalExpr (variableScopesWith argNames) (last body) of
      (result, io) -> (result, do
        -- putStrLn "functionCall evaluation"
        callVarFunc (init body) argNames
        io
        )
    Just _ -> (Nothing, exitWithErrorMessage $ "error: `" ++ fname ++ "` is not a function")
    Nothing -> case fname of
      "add" -> if length args /= 2
        then (Nothing, expectedArgs fname 2 (length args))
        else addExprs variableScopes (head args) (args !! 1)
      "sub" -> if length args /= 2
        then (Nothing, expectedArgs fname 2 (length args))
        else subExprs variableScopes (head args) (args !! 1)
      "print" -> if length args /= 1
        then (Nothing, expectedArgs "print" 1 $ length args)
        else case head args of
          ExprString str -> (Nothing, putStrLn str)
          ExprNumber str -> (Nothing, putStrLn str)
          ExprId str -> (Nothing, putStrLn str)
          fc@(FunctionCall _ _) -> case evalExpr variableScopes fc of
            (Just arg, io) -> (Nothing, do
              io
              case arg of
                VarString str -> putStrLn str
                VarNumber str -> putStrLn str
              )
            (Nothing, io) -> (Nothing, do
              io
              exitWithErrorMessage "error: argument expression wasn't evaluated"
              )
          _ -> (Nothing, exitWithErrorMessage "not implemented yet") -- TODO:
      _ -> (Nothing, exitWithErrorMessage $ "error: `" ++ fname ++ "` is not initialized")
  where callVarFunc body argNames = if length argNames /= length args
          then expectedArgs fname (length argNames) (length args)
          else do
            sequence_ ios
            evaluator' (variableScopesWith argNames) body
        mbVar = evalExpr variableScopes 
        argVars = catMaybes argMaybeVars
        (argMaybeVars, ios) = unzip evalExprs
        evalExprs = map (evalExpr variableScopes) args
        variableScopesWith argNames =
          zip argNames argVars:variableScopes
evalExpr variableScopes (PatternMatching switch cases defaultCase) = case evalExpr variableScopes expr of
  (res, io2) -> (res, do
    -- putStrLn "patternMatching evaluation"
    io
    io2
    )
  where (lefts, rights) = unzip cases
        (io, maybeIndex) = findMatch variableScopes switch lefts
        expr = case maybeIndex of
          Just i -> rights !! i
          Nothing -> defaultCase
evalExpr _ e = (Nothing, exitWithErrorMessage $ "error: this expression can't be evaluated: " ++ show e)

addExprs :: [[(String, Variable)]] -> Expr -> Expr -> (Maybe Variable, IO ())
addExprs variableScopes lhs rhs = case evalExpr variableScopes lhs of
  (Just (VarNumber lhsNum), io1) -> case evalExpr variableScopes rhs of
    (Just (VarNumber rhsNum), io2) -> if show (read lhsNum::Int) == lhsNum
      then (Just $ VarNumber $ show $ (read lhsNum::Int) + (read rhsNum::Int), return ())
      else (Just $ VarNumber $ show $ (read lhsNum::Double) + (read rhsNum::Double), return ())
    (_, io2) -> (Nothing, do
      io1
      io2
      exitWithErrorMessage "expected number argument"
      )
  (_, io1) -> (Nothing, do
    io1
    exitWithErrorMessage "expected number argument"
    )

subExprs :: [[(String, Variable)]] -> Expr -> Expr -> (Maybe Variable, IO ())
subExprs variableScopes lhs rhs = case evalExpr variableScopes lhs of
  (Just (VarNumber lhsNum), io1) -> case evalExpr variableScopes rhs of
    (Just (VarNumber rhsNum), io2) -> if show (read lhsNum::Int) == lhsNum
      then (Just $ VarNumber $ show $ (read lhsNum::Int) - (read rhsNum::Int), return ())
      else (Just $ VarNumber $ show $ (read lhsNum::Double) - (read rhsNum::Double), return ())
    (_, io2) -> (Nothing, do
      io1
      io2
      exitWithErrorMessage "expected number argument"
      )
  (_, io1) -> (Nothing, do
    io1
    exitWithErrorMessage "expected number argument"
    )
