module EvaluationTests.SimpleTests.EvaluationSimpleAnd (evalSimpleAndTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleAndTests :: Test
evalSimpleAndTests = TestList [testEvalAnd1, testEvalAnd2, testEvalAnd3, testEvalAnd4, testEvalAnd5, testEvalAnd6, testEvalAnd7, testEvalAnd8, testEvalAnd9, testEvalAnd10]

testEvalAnd1 :: Test
testEvalAnd1 = TestCase $ do
  assertEqual "Eval Test And #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 1)
    b1 = Var (AstInt 1)
    operation1 = BinaryOp And a1 b1
    result1 = Var (AstBool "#t")

testEvalAnd2 :: Test
testEvalAnd2 = TestCase $ do
  assertEqual "Eval Test And #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 1)
    b2 = Var (AstInt 0)
    operation2 = BinaryOp And a2 b2
    result2 = Var (AstBool "#f")

testEvalAnd3 :: Test
testEvalAnd3 = TestCase $ do
  assertEqual "Eval Test And #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt 0)
    b3 = Var (AstInt 1)
    operation3 = BinaryOp And a3 b3
    result3 = Var (AstBool "#f")

testEvalAnd4 :: Test
testEvalAnd4 = TestCase $ do
  assertEqual "Eval Test And #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 0)
    b4 = Var (AstInt 0)
    operation4 = BinaryOp And a4 b4
    result4 = Var (AstBool "#f")

testEvalAnd5 :: Test
testEvalAnd5 = TestCase $ do
  assertEqual "Eval Test And #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 1)
    operation5 = BinaryOp And a5 b5
    result5 = Var (AstBool "#f")

testEvalAnd6 :: Test
testEvalAnd6 = TestCase $ do
  assertEqual "Eval Test And #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 1)
    b6 = Var (AstInt 1)
    operation6 = BinaryOp And a6 b6
    result6 = Var (AstBool "#t")

testEvalAnd7 :: Test
testEvalAnd7 = TestCase $ do
  assertEqual "Eval Test And #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 0)
    operation7 = BinaryOp And a7 b7
    result7 = Var (AstBool "#f")

testEvalAnd8 :: Test
testEvalAnd8 = TestCase $ do
  assertEqual "Eval Test And #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 10)
    b8 = Var (AstInt 20)
    operation8 = BinaryOp And a8 b8
    result8 = Var (AstBool "#t")

testEvalAnd9 :: Test
testEvalAnd9 = TestCase $ do
  assertEqual "Eval Test And #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-2))
    b9 = Var (AstInt 2)
    operation9 = BinaryOp And a9 b9
    result9 = Var (AstBool "#t")

testEvalAnd10 :: Test
testEvalAnd10 = TestCase $ do
  assertEqual "Eval Test And #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstInt 0)
    operation10 = BinaryOp And a10 b10
    result10 = Var (AstBool "#f")
