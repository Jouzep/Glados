import Test.HUnit

-- import AstTest
-- import ParserTest
-- import ConvertToAstTest
import VMTests.Index

-- Combine all test cases
allTests :: Test
allTests = TestList
  [ 
    -- TestLabel "Evaluation Operator Simple tests" $ evalSimpleTestList
  -- , TestLabel "Ast tests" $ astTests
  -- , TestLabel "Parser tests" $ parserTests
  -- , TestLabel "Convert to Ast tests" $ convertToAstTests
  -- , TestLabel "StringToBinaryOp tests" $ stringToBinaryOpTests
  TestLabel "vmTestOperator tests" $ vmTestList
  ]

-- Main function to run all the defined tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()
