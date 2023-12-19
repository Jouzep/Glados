module EvaluationTests.SimpleTests.EvaluationSimpleDivide (evalSimpleDivideTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleDivideTests :: Test
evalSimpleDivideTests = TestList [testEvalDivide1, testEvalDivide2, testEvalDivide3, testEvalDivide4, testEvalDivide5, testEvalDivide6, testEvalDivide7, testEvalDivide8, testEvalDivide9, testEvalDivide10, testEvalDivide11]

testEvalDivide1 :: Test
testEvalDivide1 = TestCase $ do
  assertEqual "Eval Test Divide #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 10)
    b1 = Var (AstInt 2)
    operation1 = BinaryOp Divide a1 b1
    result1 = Var (AstInt 5)

testEvalDivide2 :: Test
testEvalDivide2 = TestCase $ do
  assertEqual "Eval Test Divide #2" (result2) (evaluation operation2)
  where
    a2 = Var (AstInt 8)
    b2 = Var (AstInt 0)
    operation2 = BinaryOp Divide a2 b2
    result2 = Nothing  -- Division by zero should return Nothing

testEvalDivide3 :: Test
testEvalDivide3 = TestCase $ do
  assertEqual "Eval Test Divide #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-15))
    b3 = Var (AstInt 3)
    operation3 = BinaryOp Divide a3 b3
    result3 = Var (AstInt (-5))

testEvalDivide4 :: Test
testEvalDivide4 = TestCase $ do
  assertEqual "Eval Test Divide #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt 5)
    operation4 = BinaryOp Divide a4 b4
    result4 = Var (AstInt 20)

testEvalDivide5 :: Test
testEvalDivide5 = TestCase $ do
  assertEqual "Eval Test Divide #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 15)
    b5 = Var (AstInt 3)
    operation5 = BinaryOp Divide a5 b5
    result5 = Var (AstInt 5)

testEvalDivide6 :: Test
testEvalDivide6 = TestCase $ do
  assertEqual "Eval Test Divide #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 25)
    b6 = Var (AstInt 5)
    operation6 = BinaryOp Divide a6 b6
    result6 = Var (AstInt 5)

testEvalDivide7 :: Test
testEvalDivide7 = TestCase $ do
  assertEqual "Eval Test Divide #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-30))
    b7 = Var (AstInt 6)
    operation7 = BinaryOp Divide a7 b7
    result7 = Var (AstInt (-5))

testEvalDivide8 :: Test
testEvalDivide8 = TestCase $ do
  assertEqual "Eval Test Divide #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 64)
    b8 = Var (AstInt 8)
    operation8 = BinaryOp Divide a8 b8
    result8 = Var (AstInt 8)

testEvalDivide9 :: Test
testEvalDivide9 = TestCase $ do
  assertEqual "Eval Test Divide #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt 20)
    b9 = Var (AstInt 2)
    operation9 = BinaryOp Divide a9 b9
    result9 = Var (AstInt 10)

testEvalDivide10 :: Test
testEvalDivide10 = TestCase $ do
  assertEqual "Eval Test Divide #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 7)
    b10 = Var (AstInt 1)
    operation10 = BinaryOp Divide a10 b10
    result10 = Var (AstInt 7)

testEvalDivide11 :: Test
testEvalDivide11 = TestCase $ do
  assertEqual "Eval Test Divide #11" (result10) (evaluation operation10)
  where
    a10 = Var (AstInt 7)
    b10 = Var (AstSymb "4s")
    operation10 = BinaryOp Divide a10 b10
    result10 = Nothing