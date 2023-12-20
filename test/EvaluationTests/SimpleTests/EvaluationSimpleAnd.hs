module EvaluationTests.SimpleTests.EvaluationSimpleAnd (evalSimpleAndTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleAndTests :: Test
evalSimpleAndTests = TestList [testEvalAnd1, testEvalAnd2, testEvalAnd3, testEvalAnd4, testEvalAnd5]

testEvalAnd1 :: Test
testEvalAnd1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test And #1" (Just expected) (result)
  where
    a = Var (AstBool "#f")
    b = Var (AstBool "#f")
    operation = BinaryOp And a b
    expected = Var (AstBool "#f")

testEvalAnd2 :: Test
testEvalAnd2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test And #2" (Just expected) (result)
  where
    a = Var (AstBool "#t")
    b = Var (AstBool "#f")
    operation = BinaryOp And a b
    expected = Var (AstBool "#f")

testEvalAnd3 :: Test
testEvalAnd3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test And #3" (Just expected) (result)
  where
    a = Var (AstBool "#f")
    b = Var (AstBool "#t")
    operation = BinaryOp And a b
    expected = Var (AstBool "#f")

testEvalAnd4 :: Test
testEvalAnd4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test And #4" (Just expected) (result)
  where
    a = Var (AstBool "#t")
    b = Var (AstBool "#t")
    operation = BinaryOp And a b
    expected = Var (AstBool "#t")

testEvalAnd5 :: Test
testEvalAnd5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test And #5" (expected) (result)
  where
    a = Var (AstBool "#t")
    b = Var (AstInt 5)
    operation = BinaryOp And a b
    expected = Nothing