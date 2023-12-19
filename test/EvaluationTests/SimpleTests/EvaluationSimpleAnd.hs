module EvaluationTests.SimpleTests.EvaluationSimpleAnd (evalSimpleAndTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleAndTests :: Test
evalSimpleAndTests = TestList [testEvalAnd1, testEvalAnd2, testEvalAnd3, testEvalAnd4, testEvalAnd5]

testEvalAnd1 :: Test
testEvalAnd1 = TestCase $ do
  assertEqual "Eval Test And #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstBool "#f")
    b1 = Var (AstBool "#f")
    operation1 = BinaryOp And a1 b1
    result1 = Var (AstBool "#f")

testEvalAnd2 :: Test
testEvalAnd2 = TestCase $ do
  assertEqual "Eval Test And #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstBool "#t")
    b2 = Var (AstBool "#f")
    operation2 = BinaryOp And a2 b2
    result2 = Var (AstBool "#f")

testEvalAnd3 :: Test
testEvalAnd3 = TestCase $ do
  assertEqual "Eval Test And #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstBool "#f")
    b3 = Var (AstBool "#t")
    operation3 = BinaryOp And a3 b3
    result3 = Var (AstBool "#f")

testEvalAnd4 :: Test
testEvalAnd4 = TestCase $ do
  assertEqual "Eval Test And #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstBool "#t")
    b4 = Var (AstBool "#t")
    operation4 = BinaryOp And a4 b4
    result4 = Var (AstBool "#t")

testEvalAnd5 :: Test
testEvalAnd5 = TestCase $ do
  assertEqual "Eval Test And #5" (result4) (evaluation operation4)
  where
    a4 = Var (AstBool "#t")
    b4 = Var (AstInt 5)
    operation4 = BinaryOp And a4 b4
    result4 = Nothing