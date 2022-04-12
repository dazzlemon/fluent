import Test.HUnit
    ( assertEqual
    , runTestTT
    , Counts(errors, failures)
    , Test(TestLabel, TestCase, TestList)
    )
import System.Exit ( exitFailure, exitSuccess )
import Lexer ( getToken
             , Token(..)
             , TokenInfo(TokenInfo)
             )

testSingleChar (char, token, string) = assertEqual (string ++ " with garbage")
  (Right (TokenInfo 0 token, "aboba")) (getToken (char:"aboba") 0)

singleCharOperatorsTest = TestCase tests
  where chars = [ ('(', ParenthesisLeft, "left parenthesis")
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
        tests = mapM_ testSingleChar chars

-- Number
-- "12345"

-- "1234.567"
-- "1234.x567" -> UnexpectedSymbol
-- "1234." -> UnexpectedEOF

-- ".1234"
-- ".x" -> UnexpectedSymbol
-- "." -> UnexpectedEOF

-- "-12345"
-- "-x" -> UnexpectedSymbol
-- "-" -> UnexpectedEOF

-- "-1234.567"
-- "-1234.x" -> UnexpectedSymbol
-- "-1234." -> UnexpectedEOF

-- "-.x" -> UnexpectedSymbol
-- "-." -> UnexpectedEOF
-- "-.1234"

-- StringLiteral
-- " 'Shsntbbtns' "
-- " ' tntn" -> UnexpectedEOF

-- Id "aboba_228" "match127" "NULLify"

-- AssignmentOperator "<-"
-- "<" -> UnexpectedEOF
-- "<x" -> UnexpectedSymbol

-- Null "NULL"
-- MatchKeyword "match"

-- MatchArrow "->" same as negative numbers

-- UnknownSymbol is triggered when encountered a symbol that is not in alphabet

-- skip comments and spaces

tests = TestList [TestLabel "singleCharOperatorsTest" singleCharOperatorsTest]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
