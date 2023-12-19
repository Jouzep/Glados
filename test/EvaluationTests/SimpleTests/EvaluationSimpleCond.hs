module EvaluationTests.SimpleTests.EvaluationSimpleCond (evalSimpleCondTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleCondTests :: Test
evalSimpleCondTests = TestList [testEvalCond1, testEvalCond2, testEvalCond3, testEvalCond4, testEvalCond5, testEvalCond6, testEvalCond7, testEvalCond8, testEvalCond9, testEvalCond10, testEvalCond11, testEvalCond12]

testEvalCond1 :: Test
testEvalCond1 = TestCase $ do
  assertEqual "Eval Test Cond #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 10)
    b1 = Var (AstInt 5)
    condition = BinaryOp LessThan a1 b1
    operation1 = Cond condition a1 b1
    result1 = b1

testEvalCond2 :: Test
testEvalCond2 = TestCase $ do
  assertEqual "Eval Test Cond #2" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 10)
    b1 = Var (AstInt 5)
    condition = BinaryOp LessThan b1 a1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond3 :: Test
testEvalCond3 = TestCase $ do
  assertEqual "Eval Test Cond #3" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 10)
    b1 = Var (AstInt 5)
    condition = BinaryOp GreaterThan a1 b1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond4 :: Test
testEvalCond4 = TestCase $ do
  assertEqual "Eval Test Cond #4" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 10)
    b1 = Var (AstInt 5)
    condition = BinaryOp GreaterThan b1 a1
    operation1 = Cond condition a1 b1
    result1 = b1

testEvalCond5 :: Test
testEvalCond5 = TestCase $ do
  assertEqual "Eval Test Cond #5" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 10)
    b1 = Var (AstInt 10)
    condition = BinaryOp GreaterThanOrEqual a1 b1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond6 :: Test
testEvalCond6 = TestCase $ do
  assertEqual "Eval Test Cond #6" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 10)
    b1 = Var (AstInt 10)
    condition = BinaryOp GreaterThanOrEqual b1 a1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond7 :: Test
testEvalCond7 = TestCase $ do
  assertEqual "Eval Test Cond #7" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 10)
    condition = BinaryOp LessThanOrEqual a1 b1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond8 :: Test
testEvalCond8 = TestCase $ do
  assertEqual "Eval Test Cond #8" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 10)
    condition = BinaryOp LessThanOrEqual b1 a1
    operation1 = Cond condition a1 b1
    result1 = b1

testEvalCond9 :: Test
testEvalCond9 = TestCase $ do
  assertEqual "Eval Test Cond #9" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    condition = BinaryOp Equal a1 b1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond10 :: Test
testEvalCond10 = TestCase $ do
  assertEqual "Eval Test Cond #10" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    condition = BinaryOp Equal b1 a1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond11 :: Test
testEvalCond11 = TestCase $ do
  assertEqual "Eval Test Cond #11" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 10)
    condition = BinaryOp NotEqual a1 b1
    operation1 = Cond condition a1 b1
    result1 = a1

testEvalCond12 :: Test
testEvalCond12 = TestCase $ do
  assertEqual "Eval Test Cond #12" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 10)
    condition = BinaryOp NotEqual b1 a1
    operation1 = Cond condition a1 b1
    result1 = a1