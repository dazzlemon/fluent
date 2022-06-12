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

main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    [filename] -> runCode filename False
    [filename, "--dev"] -> runCode filename True
    _ -> do
      putStrLn "incorrect usage\n"
      showHelp
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
      when isDev printTokenList
      case parser tokens of
        Right commands -> do
          when isDev (printAst commands)
          mbStackTrace <- evaluator commands
          forM_ mbStackTrace (printStackTrace code)
        Left (pos, err) -> do
          putStrLn $ "parser error at " ++ show pos ++ ": " ++ show err
          putStr $ showErr code (position $ tokenList !! pos)
      where printTokenList = putStr $ showTable
                                    $ tokenInfoListToTable tokenList
            printAst commands = do
              putStrLn ""
              mapM_ (printExpr 0) commands
              putStrLn ""
            tokens = map token tokenList
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
            printExpr n (e, pos) = case e of
              Assignment lhs rhs -> do
                putStrLn0p0 $ "assignment to `" ++ lhs ++ "`:"
                printExpr1 rhs
              ExprId str -> putStrLn0 $ "id: " ++ str
              ExprNumber str -> putStrLn0 $ "number: " ++ str
              FunctionCall fname args -> do
                putStrLn0p0 $ "function call `" ++ fname ++ "`, args:"
                mapM_ printExpr1 args
              NamedTuppleAccess lhs rhs -> do
                putStrLn0p0 $ "named tuple acess `" ++ lhs ++ ":" ++ rhs ++ "`"
              Tuple fields -> do
                putStrLn0 "tuple:"
                mapM_ printExpr1 fields
              NamedTuple fields -> do
                putStrLn0 "named tuple:"
                mapM_ (printNamedTupleField (n + 1)) fields
              LambdaDef args body -> do
                putStrLn0 "lambda:"
                putStrLn1 $ "args: " ++ show args
                putStrLn1 "body:"
                mapM_ printExpr2 body
              PatternMatching switch cases defaultCase -> do
                putStrLn0 "pattern matching:"
                putStrLn1 "switch:"
                printExpr2 switch
                putStrLn1 "cases:"
                mapM_ (printCase (n + 2)) cases
                putStrLn1 "default:"
                printExpr2 defaultCase
              _ -> putStrLn0 $ show e
              where putStrLn' n s = putStrLn $ replicate n '\t' ++ s
                    putStrLn0 = putStrLn' n
                    putStrLn1 = putStrLn' (n + 1)
                    putStrLn2 = putStrLn' (n + 2)
                    printExpr1 = printExpr (n + 1)
                    printExpr2 = printExpr (n + 2)
                    withPos pos s = show pos ++ " | " ++ s
                    putStrLn0p pos s = putStrLn0 $ withPos pos s
                    putStrLn0p0 = putStrLn0p pos
                    printNamedTupleField n (lhs, rhs) = do
                      putStrLn0 $ "field `" ++ lhs ++ "`:"
                      printExpr (n + 1) rhs
                    printCase n (lhs, rhs) = do
                      putStrLn0 "case:"
                      putStrLn1 "lhs:"
                      printExpr2 lhs
                      putStrLn1 "rhs:"
                      printExpr2 rhs

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