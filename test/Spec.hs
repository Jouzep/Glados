import Test.HUnit

import AstTest
import ParserTest
import EvaluationTests.EvaluationSimpleOperator

-- Combine all test cases
allTests :: Test
allTests = TestList
  [ 
    -- TestLabel "Ast tests" $ astTests,
    -- TestLabel "Parser tests" $ parserTests,
    TestLabel "Evaluation Simple Operator tests" $ evalSimpleOperatorTests
  ]

-- Main function to run all the defined tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()