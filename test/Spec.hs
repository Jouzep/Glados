import Test.HUnit

import AstTest
import ParserTest
import ConvertToAstTest
import EvaluationTests.SimpleTests.EvaluationSimpleTests

-- Combine all test cases
allTests :: Test
allTests = TestList
  [ TestLabel "Ast tests" $ astTests
  , TestLabel "Parser tests" $ parserTests
  , TestLabel "Convert to Ast tests" $ convertToAstTests
  , TestLabel "StringToBinaryOp tests" $ stringToBinaryOpTests
  , TestLabel "Evaluation Operator Simple tests" $ evalSimpleTestList
  ]

-- Main function to run all the defined tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
