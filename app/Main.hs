import Data.Data (Data(toConstr))
import Lexer
import Control.Monad (when, forM_)
import System.Exit (exitFailure)
import Parser
import Evaluator
import System.Environment
import Stuff
import ListPadding
import Data.List (elemIndices)
import Data.Text (pack, unpack, replace)

-- main = interact (show . lexer) -- normal version
main = do -- pretty print version
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    [filename] -> runCode filename False
    [filename, "--dev"] -> runCode filename True
    _ -> showHelp
  where showHelp = do
          putStrLn "fluent interpreter"
          putStrLn "Usage:"
          putStrLn "fluent --help"
          putStrLn "\twill show you this screen"
          putStrLn "fluent filename [--dev]"
          putStrLn "\twill run the program stored in filename,"
          putStrLn "\tif --dev is provided"
          putStrLn "\tthen interpreter will print token list"
          putStrLn "\tand AST before running the program"
        runCode filename isDev = do
          code' <- readFile filename
          -- replace tabs with 4 spaces for better printing of errors later
          let code = unpack (replace (pack "\t") (pack "    ") (pack code'))
          -- code <- getContents
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
              when isDev (putStr $ showTable
                                 $ tokenInfoListToTable tokenList
                         )
              case parser tokens of
                Right commands -> do
                  when isDev (do
                    putStrLn ""
                    mapM_ (printExpr 0) commands
                    putStrLn ""
                    )
                  mbStackTrace <- evaluator commands
                  forM_ mbStackTrace (printStackTrace code)
                Left (pos, err) -> do
                  putStrLn $ "parser error at " ++ show pos ++ ": " ++ show err
                  putStr $ showErr code (position $ tokenList !! pos)
              where tokens = map token tokenList
                    printExpr n (e, pos) = case e of
                      Assignment (ExprId lhs, pos1) rhs -> do
                        putStrLn $ replicate n '\t' ++ show pos ++ " | assignment to `" ++ lhs ++ "`:"
                        printExpr (n + 1) rhs
                      ExprId str -> putStrLn $ replicate n '\t' ++ "id: " ++ str
                      ExprNumber str -> putStrLn $ replicate n '\t' ++ "number: " ++ str
                      FunctionCall (ExprId fname, pos1) args -> do
                        putStrLn $ replicate n '\t' ++ show pos ++ " | function call `" ++ fname ++ "`, args:"
                        mapM_ (printExpr (n + 1)) args
                      NamedTuppleAccess (ExprId lhs, pos1) (ExprId rhs, pos2) -> do
                        putStrLn $ replicate n '\t' ++ show pos ++
                          " | named tuple acess `" ++ lhs ++ ":" ++ rhs ++ "`"
                      Tuple fields -> do
                        putStrLn $ replicate n '\t' ++ "tuple:"
                        mapM_ (printExpr (n + 1)) fields
                      NamedTuple fields -> do
                        putStrLn $ replicate n '\t' ++ "named tuple:"
                        mapM_ (printNamedTupleField (n + 1)) fields
                      LambdaDef args body -> do
                        putStrLn $ replicate n '\t' ++ "lambda:"
                        putStrLn $ replicate (n + 1) '\t' ++ "args: " ++ show (map (str . fst) args)
                        putStrLn $ replicate (n + 1) '\t' ++ "body:"
                        mapM_ (printExpr (n + 2)) body
                      PatternMatching switch cases defaultCase -> do
                        putStrLn $ replicate n '\t' ++ "pattern matching:"
                        putStrLn $ replicate (n + 1) '\t' ++ "switch:"
                        printExpr (n + 2) switch
                        putStrLn $ replicate (n + 1) '\t' ++ "cases:"
                        mapM_ (printCase (n + 2)) cases
                        putStrLn $ replicate (n + 1) '\t' ++ "default:"
                        printExpr (n + 2) defaultCase
                      _ -> putStrLn $ replicate n '\t' ++ show e
                    printNamedTupleField n ((ExprId lhs, pos1), rhs) = do
                      putStrLn $ replicate n '\t' ++ show pos1 ++ " | field `" ++ lhs ++ "`:"
                      printExpr (n + 1) rhs
                    printCase n (lhs, rhs) = do
                      putStrLn $ replicate n '\t' ++ "case:"
                      putStrLn $ replicate (n + 1) '\t' ++ "lhs:"
                      printExpr (n + 2) lhs
                      putStrLn $ replicate (n + 1) '\t' ++ "rhs:"
                      printExpr (n + 2) rhs
                    printStackTrace code error = case error of
                      InitialError p w -> do
                        putStrLn $ "error: " ++ w
                        putStrLn $ showErr code (position (tokenList !! p))
                      StackTrace p w e -> do
                        putStrLn $ "error in: " ++ w
                        putStrLn $ showErr code (position (tokenList !! p))
                        putStrLn "trace:"
                        putStrLn ""
                        printStackTrace code e

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