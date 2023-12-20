module EvaluationTests.SimpleTests.EvaluationSimpleCond (evalSimpleCondTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleCondTests :: Test
evalSimpleCondTests = TestList [testEvalCond1, testEvalCond2, testEvalCond3, testEvalCond4, testEvalCond5, testEvalCond6, testEvalCond7, testEvalCond8, testEvalCond9, testEvalCond10, testEvalCond11, testEvalCond12]

testEvalCond1 :: Test
testEvalCond1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #1" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 5)
    condition = BinaryOp LessThan a b
    operation = Cond condition a b
    expected = b

testEvalCond2 :: Test
testEvalCond2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #2" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 5)
    condition = BinaryOp LessThan b a
    operation = Cond condition a b
    expected = a

testEvalCond3 :: Test
testEvalCond3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #3" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 5)
    condition = BinaryOp GreaterThan a b
    operation = Cond condition a b
    expected = a

testEvalCond4 :: Test
testEvalCond4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #4" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 5)
    condition = BinaryOp GreaterThan b a
    operation = Cond condition a b
    expected = b

testEvalCond5 :: Test
testEvalCond5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #5" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 10)
    condition = BinaryOp GreaterThanOrEqual a b
    operation = Cond condition a b
    expected = a

testEvalCond6 :: Test
testEvalCond6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #6" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 10)
    condition = BinaryOp GreaterThanOrEqual b a
    operation = Cond condition a b
    expected = a

testEvalCond7 :: Test
testEvalCond7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #7" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 10)
    condition = BinaryOp LessThanOrEqual a b
    operation = Cond condition a b
    expected = a

testEvalCond8 :: Test
testEvalCond8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #8" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 10)
    condition = BinaryOp LessThanOrEqual b a
    operation = Cond condition a b
    expected = b

testEvalCond9 :: Test
testEvalCond9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #9" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    condition = BinaryOp Equal a b
    operation = Cond condition a b
    expected = a

testEvalCond10 :: Test
testEvalCond10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #10" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    condition = BinaryOp Equal b a
    operation = Cond condition a b
    expected = a

testEvalCond11 :: Test
testEvalCond11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #11" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 10)
    condition = BinaryOp NotEqual a b
    operation = Cond condition a b
    expected = a

testEvalCond12 :: Test
testEvalCond12 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Cond #12" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 10)
    condition = BinaryOp NotEqual b a
    operation = Cond condition a b
    expected = a