module EvaluationTests.SimpleTests.EvaluationSimpleDivide (evalSimpleDivideTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleDivideTests :: Test
evalSimpleDivideTests = TestList [testEvalDivide1, testEvalDivide2, testEvalDivide3, testEvalDivide4, testEvalDivide5, testEvalDivide6, testEvalDivide7, testEvalDivide8, testEvalDivide9, testEvalDivide10, testEvalDivide11]

testEvalDivide1 :: Test
testEvalDivide1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #1" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 2)
    operation = BinaryOp Divide a b
    expected = Var (AstInt 5)

testEvalDivide2 :: Test
testEvalDivide2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #2" (expected) (result)
  where
    a = Var (AstInt 8)
    b = Var (AstInt 0)
    operation = BinaryOp Divide a b
    expected = Nothing  -- Division by zero should return Nothing

testEvalDivide3 :: Test
testEvalDivide3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #3" (Just expected) (result)
  where
    a = Var (AstInt (-15))
    b = Var (AstInt 3)
    operation = BinaryOp Divide a b
    expected = Var (AstInt (-5))

testEvalDivide4 :: Test
testEvalDivide4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt 5)
    operation = BinaryOp Divide a b
    expected = Var (AstInt 20)

testEvalDivide5 :: Test
testEvalDivide5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #5" (Just expected) (result)
  where
    a = Var (AstInt 15)
    b = Var (AstInt 3)
    operation = BinaryOp Divide a b
    expected = Var (AstInt 5)

testEvalDivide6 :: Test
testEvalDivide6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #6" (Just expected) (result)
  where
    a = Var (AstInt 25)
    b = Var (AstInt 5)
    operation = BinaryOp Divide a b
    expected = Var (AstInt 5)

testEvalDivide7 :: Test
testEvalDivide7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #7" (Just expected) (result)
  where
    a = Var (AstInt (-30))
    b = Var (AstInt 6)
    operation = BinaryOp Divide a b
    expected = Var (AstInt (-5))

testEvalDivide8 :: Test
testEvalDivide8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #8" (Just expected) (result)
  where
    a = Var (AstInt 64)
    b = Var (AstInt 8)
    operation = BinaryOp Divide a b
    expected = Var (AstInt 8)

testEvalDivide9 :: Test
testEvalDivide9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #9" (Just expected) (result)
  where
    a = Var (AstInt 20)
    b = Var (AstInt 2)
    operation = BinaryOp Divide a b
    expected = Var (AstInt 10)

testEvalDivide10 :: Test
testEvalDivide10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #10" (Just expected) (result)
  where
    a = Var (AstInt 7)
    b = Var (AstInt 1)
    operation = BinaryOp Divide a b
    expected = Var (AstInt 7)

testEvalDivide11 :: Test
testEvalDivide11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Divide #11" (expected) (result)
  where
    a = Var (AstInt 7)
    b = Var (AstSymb "4s")
    operation = BinaryOp Divide a b
    expected = Nothing