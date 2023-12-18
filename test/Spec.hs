import Test.HUnit

import AstTest
import ParserTest


-- Combine all test cases
allTests :: Test
allTests = TestList
  [ TestLabel "Ast tests" $ astTests
  , TestLabel "Parser tests" $ parserTests
  ]

-- Main function to run all the defined tests
main :: IO ()
main = do
  _ <- runTestTT allTests
  return ()