-- test/ArithmeticTests.hs

module AstTest where

import Test.HUnit
import Lib

astTests :: Test
astTests = TestList [testAddition, testSubtraction]



testAddition :: Test
testAddition = TestCase $ do
  assertEqual "Ast Test #1" 5 (add 2 3)

testSubtraction :: Test
testSubtraction = TestCase $ do
  assertEqual "Ast Test #2" 2 (subtract 5 3)
