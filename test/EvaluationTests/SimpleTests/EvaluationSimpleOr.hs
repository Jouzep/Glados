module EvaluationTests.SimpleTests.EvaluationSimpleOr (evalSimpleOrTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleOrTests :: Test
evalSimpleOrTests = TestList [testEvalOr1, testEvalOr2, testEvalOr3, testEvalOr4, testEvalOr5, testEvalOr6, testEvalOr7, testEvalOr8, testEvalOr9, testEvalOr10]

testEvalOr1 :: Test
testEvalOr1 = TestCase $ do
  assertEqual "Eval Test Or #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 0)
    b1 = Var (AstInt 0)
    operation1 = BinaryOp Or a1 b1
    result1 = Var (AstBool "#f")

testEvalOr2 :: Test
testEvalOr2 = TestCase $ do
  assertEqual "Eval Test Or #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 1)
    operation2 = BinaryOp Or a2 b2
    result2 = Var (AstBool "#t")

testEvalOr3 :: Test
testEvalOr3 = TestCase $ do
  assertEqual "Eval Test Or #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt 1)
    b3 = Var (AstInt 0)
    operation3 = BinaryOp Or a3 b3
    result3 = Var (AstBool "#t")

testEvalOr4 :: Test
testEvalOr4 = TestCase $ do
  assertEqual "Eval Test Or #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 1)
    b4 = Var (AstInt 1)
    operation4 = BinaryOp Or a4 b4
    result4 = Var (AstBool "#t")

testEvalOr5 :: Test
testEvalOr5 = TestCase $ do
  assertEqual "Eval Test Or #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt (-5))
    b5 = Var (AstInt 3)
    operation5 = BinaryOp Or a5 b5
    result5 = Var (AstBool "#t")

testEvalOr6 :: Test
testEvalOr6 = TestCase $ do
  assertEqual "Eval Test Or #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstBool ("#t"))
    b6 = Var (AstBool ("#t"))
    operation6 = BinaryOp Or a6 b6
    result6 = Var (AstBool "#t")

testEvalOr7 :: Test
testEvalOr7 = TestCase $ do
  assertEqual "Eval Test Or #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt 15)
    b7 = Var (AstInt 0)
    operation7 = BinaryOp Or a7 b7
    result7 = Var (AstBool "#t")

testEvalOr8 :: Test
testEvalOr8 = TestCase $ do
  assertEqual "Eval Test Or #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 0)
    b8 = Var (AstInt 0)
    operation8 = BinaryOp Or a8 b8
    result8 = Var (AstBool "#f")

testEvalOr9 :: Test
testEvalOr9 = TestCase $ do
  assertEqual "Eval Test Or #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-10))
    b9 = Var (AstInt 10)
    operation9 = BinaryOp Or a9 b9
    result9 = Var (AstBool "#t")

testEvalOr10 :: Test
testEvalOr10 = TestCase $ do
  assertEqual "Eval Test Or #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstBool ("#t"))
    b10 = Var (AstBool ("#t"))
    operation10 = BinaryOp Or a10 b10
    result10 = Var (AstBool "#t")
