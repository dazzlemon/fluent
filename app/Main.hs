import Data.List (transpose, elemIndices)
import Data.Data (Data(toConstr))
import Lexer
import Control.Monad ( when )
import System.Exit (exitFailure)
import ListPadding (rpad, lpad)
import Parser

-- main = interact (show . lexer) -- normal version
main = do -- pretty print version
  code <- getContents
  case lexer code of
    Left err -> do
      putStrLn $ "Error: " ++ constrString
      -- UnexpectedEOF only happens at the end of file))
      when (constrString /= "UnexpectedEOF") $
        putStr $ showErr code (errorPosition err)
      when (constrString == "UnexpectedSymbol") $
        putStrLn $ expected err
      when (constrString == "UnexpectedEOF") $
        putStrLn $ expected err
      exitFailure
      where constrString = show $ toConstr err
    Right tokenList -> do
      putStr $ showTable
             $ tokenInfoListToTable tokenList
      case parser tokens of
        Right commands -> mapM_ (printExpr 0) commands
        Left (pos, err) -> do
          putStrLn $ "parser error at " ++ show pos ++ ": " ++ show err
          putStr $ showErr code (position $ tokenList !! pos)
      where tokens = map token tokenList
            printExpr n e = case e of
              Assignment (ExprId lhs) rhs -> do
                putStrLn $ replicate n '\t' ++ "assignment to `" ++ lhs ++ "`:"
                printExpr (n + 1) rhs
              ExprId str -> putStrLn $ replicate n '\t' ++ "id: " ++ str
              ExprNumber str -> putStrLn $ replicate n '\t' ++ "number: " ++ str
              FunctionCall (ExprId fname) args -> do
                putStrLn $ replicate n '\t' ++ "function call `" ++ fname ++ "`, args:"
                mapM_ (printExpr (n + 1)) args
              _ -> putStrLn $ replicate n '\t' ++ show e

showErr code offset = showTable [ [lineIndexStr, " | ", line]
                                , ["",           "",    arrow]
                                ]
  where lineIndexStr = show $ lineNumber + 1 -- 0 based + 1
        lineNumber = length linesStarts
        lineStart = last linesStarts
        line = takeWhile (/= '\n')
             $ drop lineStart code -- drop lines before
        arrow = lpad (charInLinePos + 1) ' ' "^"
        linesStarts = 0:map (+1) ( drop 1 -- no line after last '\n'
                                          -- 0 is start of first line
                                 $ elemIndices '\n'
                                 $ take offset code) -- drop other lines
        charInLinePos = offset - lineStart

tokenInfoListToTable :: [TokenInfo] -> [[String]]
tokenInfoListToTable = map tokenInfoToRow
  where tokenInfoToRow (TokenInfo position token) = [ positionString
                                                    , constrString
                                                    , extendedInfo
                                                    ]
          where constrString = show $ toConstr token
                extendedInfo = if constrString `elem` [ "Number"
                                                      , "StringLiteral"
                                                      , "Id"
                                                      ]
                  then "\'" ++ tokenString token ++ "\'"
                  else ""
                positionString = show position

showTable :: [[String]] -> String
showTable table = unlines $ map showRow table
  where maxLengths = map (maximum . map length) $ transpose table
        showRow = unwords . zipWith (`rpad` ' ') maxLengths