-- test/Spec.hs

module Main where

import Test.HUnit
import Lib

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