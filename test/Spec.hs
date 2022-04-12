import Test.HUnit
    ( assertEqual
    , runTestTT
    , Counts(errors, failures)
    , Test(TestLabel, TestCase, TestList)
    )
import System.Exit ( exitFailure, exitSuccess )
import Lexer ( getToken, Token(..), TokenInfo(TokenInfo), LexerError(..) )
import Control.Monad
import Control.Applicative ((<|>))

testSingleChar (char, token, string) = assertEqual (string ++ " with garbage")
  (Right (TokenInfo 0 token, "aboba")) (getToken (char:"aboba") 0)

testUnknownSymbol char = assertEqual ("'" ++ [char] ++ "' with garbage")
  (Left (UnknownSymbol 0)) (getToken (char:"aboba") 0)

singleCharTokensTest = TestCase tests
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
        correctCases = map testSingleChar chars
        -- few test for UnknownSymbol,
        -- basically the only error that can happen
        -- when checking single character tokens
        unexpectedSymbols = map testUnknownSymbol "!@$%^&*"
        tests = mconcat $ unexpectedSymbols ++ correctCases

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

tests = TestList [TestLabel "singleCharTokensTest" singleCharTokensTest]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
