import Test.HUnit

import EvaluationTests.SimpleTests.EvaluationSimpleTests

-- Combine all test cases
allTests :: Test
allTests = TestList
  [ 
    TestLabel "Evaluation Operator Simple tests" $ evalSimpleTestList
  ]

-- Main function to run all the defined tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()