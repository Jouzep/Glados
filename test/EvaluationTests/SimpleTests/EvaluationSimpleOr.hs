module EvaluationTests.SimpleTests.EvaluationSimpleOr (evalSimpleOrTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleOrTests :: Test
evalSimpleOrTests = TestList [testEvalOr1, testEvalOr2, testEvalOr3, testEvalOr4, testEvalOr5]

testEvalOr1 :: Test
testEvalOr1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Or #1" (Just expected) (result)
  where
    a = Var (AstBool "#f")
    b = Var (AstBool "#f")
    operation = BinaryOp Or a b
    expected = Var (AstBool "#f")

testEvalOr2 :: Test
testEvalOr2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Or #2" (Just expected) (result)
  where
    a = Var (AstBool "#f")
    b = Var (AstBool "#t")
    operation = BinaryOp Or a b
    expected = Var (AstBool "#t")


testEvalOr3 :: Test
testEvalOr3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Or #3" (Just expected) (result)
  where
    a = Var (AstBool "#t")
    b = Var (AstBool "#f")
    operation = BinaryOp Or a b
    expected = Var (AstBool "#t")

testEvalOr4 :: Test
testEvalOr4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Or #4" (Just expected) (result)
  where
    a = Var (AstBool "#t")
    b = Var (AstBool "#t")
    operation = BinaryOp Or a b
    expected = Var (AstBool "#t")

testEvalOr5 :: Test
testEvalOr5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Or #5" (expected) (result)
  where
    a = Var (AstBool "#t")
    b = Var (AstInt 54)
    operation = BinaryOp Or a b
    expected = Nothing