module EvaluationTests.SimpleTests.EvaluationSimpleOr (evalSimpleOrTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleOrTests :: Test
evalSimpleOrTests = TestList [testEvalOr1, testEvalOr2, testEvalOr3, testEvalOr4, testEvalOr5]

testEvalOr1 :: Test
testEvalOr1 = TestCase $ do
  assertEqual "Eval Test Or #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstBool "#f")
    b1 = Var (AstBool "#f")
    operation1 = BinaryOp Or a1 b1
    result1 = Var (AstBool "#f")

testEvalOr2 :: Test
testEvalOr2 = TestCase $ do
  assertEqual "Eval Test Or #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstBool "#f")
    b2 = Var (AstBool "#t")
    operation2 = BinaryOp Or a2 b2
    result2 = Var (AstBool "#t")


testEvalOr3 :: Test
testEvalOr3 = TestCase $ do
  assertEqual "Eval Test Or #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstBool "#t")
    b3 = Var (AstBool "#f")
    operation3 = BinaryOp Or a3 b3
    result3 = Var (AstBool "#t")

testEvalOr4 :: Test
testEvalOr4 = TestCase $ do
  assertEqual "Eval Test Or #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstBool "#t")
    b4 = Var (AstBool "#t")
    operation4 = BinaryOp Or a4 b4
    result4 = Var (AstBool "#t")

testEvalOr5 :: Test
testEvalOr5 = TestCase $ do
  assertEqual "Eval Test Or #5" (result4) (evaluation operation4)
  where
    a4 = Var (AstBool "#t")
    b4 = Var (AstInt 54)
    operation4 = BinaryOp Or a4 b4
    result4 = Nothing