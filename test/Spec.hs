import Test.HUnit
    ( assertEqual
    , runTestTT
    , Counts(errors, failures)
    , Test(TestLabel, TestCase, TestList), runTestTTAndExit
    )
import System.Exit ( exitFailure, exitSuccess )
import Lexer ( getToken, Token(..), TokenInfo(TokenInfo), LexerError(..) )
import Control.Monad
import Data.List

singleCharTokensTest = TestCase tests
  where tests = mconcat $ unknownSymbols ++ correctCases

        chars = [ ('(', ParenthesisLeft, "left parenthesis")
                , (')', ParenthesisRight,  "right parenthesis")
                , ('{', BraceLeft, "left brace")
                , ('}', BraceRight, "right brace")
                , ('[', BracketLeft, "left bracket")
                , (']', BracketRight, "right bracket")
                , (';', Semicolon, "semicolon")
                , ('_', WildCard, "wildcard")
                , (':', NamedTuppleAccessOperator,
                    "named tupple acess operator")
                , ('=', NamedTuppleBindingOperator,
                    "named tupple binding operator")
                ]
        correctCases = map testSingleChar chars
        testSingleChar (char, token, string) = assertEqual
          (string ++ " with garbage")
          (Right (TokenInfo 0 token, "aboba")) (getToken (char:"aboba") 0)

        -- few test for UnknownSymbol,
        -- basically the only error that can happen
        -- when checking single character tokens
        unknownSymbols = map testUnknownSymbol "!@$%^&*"
        testUnknownSymbol char = assertEqual ("'" ++ [char] ++ "' with garbage")
          (Left (UnknownSymbol 0)) (getToken (char:"aboba") 0)

numberTest = TestCase tests
  where tests = mconcat $ correctCases
                       ++ unexpectedSymbols
                       ++ unexpectedEOFs

        correctCases = map testNumber numbers
        numbers = positive ++ negative
        positive = [ "12345"
                   , "1234.567"
                   , ".1234"
                   ]
        negative = map ('-':) positive
        testNumber numStr = assertEqual (numStr ++ " with garbage")
          (Right (TokenInfo 0 (Number numStr), "aboba"))
          (getToken (numStr ++ "aboba") 0)

        -- UnknownSymbol can't happen because it will be treated as
        -- either UnknownSymbol or end of token

        unexpectedSymbols = unexpectedSymbolsAfterDot
                         ++ unexpectedSymbolsAfterDash
        -- UnexpectedSymbol happens on non numeric symbols after '.'
        unexpectedSymbolsAfterDotStrs = map (insertAfter '.' 'x')
                                      $ filter ('.' `elem`) numbers
        unexpectedSymbolsAfterDot = map testUnexpectedSymbolsAfterDot
          unexpectedSymbolsAfterDotStrs
        testUnexpectedSymbolsAfterDot str = assertEqual (str ++ " with garbage")
          (Left (UnexpectedSymbol (n + 1) "numeric"))
          (getToken (str ++ "aboba") 0)
          where Just n = elemIndex '.' str
        -- or after '-' (numerics or '>' are expected)
        unexpectedSymbolsAfterDashStrs = map (insertAt 1 'x') negative
        unexpectedSymbolsAfterDash = map testUnexpectedSymbolsAfterDash
          unexpectedSymbolsAfterDashStrs
        testUnexpectedSymbolsAfterDash str = assertEqual
          (str ++ " with garbage")
          (Left (UnexpectedSymbol 2 "numeric or '>'"))
          (getToken (str ++ "aboba") 0)

        unexpectedEOFs = unexpectedEOFAfterDash:unexpectedEOFsAfterDot
        -- UnexpectedEOF happens if EOF is just after '.'
        unexpectedEOFsAfterDotStrs = map ((++".") . takeWhile (/= '.'))
                                   $ filter ('.' `elem`) numbers
        unexpectedEOFsAfterDot = map testUnexpectedEOFsAfterDotStrs
          unexpectedEOFsAfterDotStrs
        testUnexpectedEOFsAfterDotStrs str = assertEqual str
          (Left (UnexpectedEOF "numeric"))
          (getToken str 0)
        -- or just after '-'
        unexpectedEOFAfterDash = assertEqual "\"-\" - UnexpectedEOF"
          (Left (UnexpectedEOF "numeric or '>'"))
          (getToken "-" 0)

        insertAfter x y xs = insertAt (n + 1) y xs
          where Just n = elemIndex x xs
        insertAt n x xs = ls ++ (x:rs)
          where (ls, rs) = splitAt n xs

stringLiteralTests = TestCase tests
  where tests = mplus good unexpectedEOF
        good = assertEqual " 'Shsntbbtns' with garbage"
          (Right (TokenInfo 0 (StringLiteral "Shsntbbtns"), "aboba"))
          (getToken "'Shsntbbtns'aboba" 0)
        unexpectedEOF = assertEqual "'Shsntbbtns"
          (Left $ UnexpectedEOF "\'")
          (getToken "'Shsntbbtns" 0)

idTests = TestCase tests
  where tests = mplus normal collision
        normal = assertEqual "id 'abobA_228' with garbage"
          (Right (TokenInfo 0 (Id "abobA_228"), " arstasrta"))
          (getToken "abobA_228 arstasrta" 0)
        collision = mplus collisionMatch collisionNull
        collisionMatch = assertEqual "id 'match127' with garbage"
          (Right (TokenInfo 0 (Id "match127"), " arstasrta"))
          (getToken "match127 arstasrta" 0)
        collisionNull = assertEqual "id 'NULLify' with garbage"
          (Right (TokenInfo 0 (Id "NULLify"), " arstasrta"))
          (getToken "NULLify arstasrta" 0)

assignmentOperatorTest = TestCase tests
  where tests = mconcat [good, unexpectedEOF, unexpectedSymbol]
        good = assertEqual "<- with garbage"
          (Right (TokenInfo 0 AssignmentOperator, "saor"))
          (getToken "<-saor" 0) 
        unexpectedEOF = assertEqual "'<' without '-'"
          (Left (UnexpectedEOF "-"))
          (getToken "<" 0)
        unexpectedSymbol = assertEqual "'<' with 'x'"
          (Left (UnexpectedSymbol 1 "-"))
          (getToken "<x" 0)

-- Null "NULL"
-- MatchKeyword "match"

-- MatchArrow "->" same as negative numbers

-- UnknownSymbol is triggered when encountered a symbol that is not in alphabet

-- skip comments and spaces

tests = TestList testLabels
  where testLabels = map (uncurry TestLabel) tests
        tests = [ ("singleCharTokensTest", singleCharTokensTest)
                , ("numberTest", numberTest)
                , ("stringLiteralTests", stringLiteralTests)
                , ("idTests", idTests)
                , ("assignmentOperatorTest", assignmentOperatorTest)
                ]

main = runTestTTAndExit tests