import Test.HUnit

import EvaluationTests.SimpleTests.EvaluationSimpleTests
import EvaluationTests.ComplexTests.EvaluationComplexTests

-- Combine all test cases
allTests :: Test
allTests = TestList
  [
    TestLabel "Evaluation Operator Simple tests" $ evalSimpleTestList,
    TestLabel "Evaluation Operator Complex tests" $ evalComplexTests
  ]

-- Main function to run all the defined tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
