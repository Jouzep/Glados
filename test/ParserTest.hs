
module ParserTest (parserTests) where
import Parser.Parser
import Test.HUnit
import AST.Constants

parserTests :: Test
parserTests = TestList [testParser1, testParser2]

testParser1:: Test
testParser1 = TestCase $ do
  let result = parser input
  assertEqual "Eval Test Add #1" (Just expected) (result)
  where
    input = "a"
    expected = SSymb "a"

testParser2:: Test
testParser2 = TestCase $ do
  let result = parser input
  assertEqual "Eval Test Add #2" (Just expected) (result)
  where
    input = "1"
    expected = SInt 1