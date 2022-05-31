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
        putStr $ showTable [ [lineIndexStr, " | ", line]
                           , ["",           "",    arrow]
                           ]
      when (constrString == "UnexpectedSymbol") $
        putStrLn $ expected err
      when (constrString == "UnexpectedEOF") $
        putStrLn $ expected err
      exitFailure
      where constrString = show $ toConstr err
            lineIndexStr = show $ lineNumber + 1 -- 0 based + 1
            lineStart = last linesStarts
            line = takeWhile (/= '\n')
                 $ drop lineStart code -- drop lines before 
            arrow = lpad (charInLinePos + 1) ' ' "^"
            offset = errorPosition err
            linesStarts = 0:map (+1) ( drop 1 -- no line after last '\n'
                                              -- 0 is start of first line
                                     $ elemIndices '\n'
                                     $ take offset code) -- drop other lines
            charInLinePos = offset - lineStart
            lineNumber = length linesStarts
    Right tokenList -> do
      putStr $ showTable
             $ tokenInfoListToTable tokenList
      case parser tokens of
        Right commands -> print commands
        _ -> return ()
      where tokens = map token tokenList

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