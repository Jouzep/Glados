
module AstTest (astTests) where

import Test.HUnit
import Lib

astTests :: Test
astTests = TestList [testAddition, testAdditionOne]



testAddition :: Test
testAddition = TestCase $ do
  assertEqual "Ast Test #1" 5 (add 2 3)

testAdditionOne :: Test
testAdditionOne = TestCase $ do
  assertEqual "Ast Test #2" 3 (add 3 3)

