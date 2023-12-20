import Test.HUnit

import EvaluationTests.SimpleTests.EvaluationSimpleTests
import AstTest
import ParserTest
import ConvertToAstTest

-- Combine all test cases
allTests :: Test
allTests = TestList
  [ TestLabel "Evaluation Operator Simple tests" $ evalSimpleTestList
  , TestLabel "Ast tests" $ astTests
  , TestLabel "Parser tests" $ parserTests
  , TestLabel "Convert to Ast tests" $ convertToAstTests
  , TestLabel "StringToBinaryOp tests" $ stringToBinaryOpTests
  ]

-- Main function to run all the defined tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
