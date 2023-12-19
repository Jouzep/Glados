module EvaluationTests.SimpleTests.EvaluationSimpleEqual (evalSimpleEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleEqualTests :: Test
evalSimpleEqualTests = TestList [testEvalEqual1, testEvalEqual2, testEvalEqual3, testEvalEqual4, testEvalEqual5, testEvalEqual6, testEvalEqual7, testEvalEqual8, testEvalEqual9, testEvalEqual10]

testEvalEqual1 :: Test
testEvalEqual1 = TestCase $ do
  assertEqual "Eval Test Equal #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    operation1 = BinaryOp Equal a1 b1
    result1 = Var (AstBool "#t")

testEvalEqual2 :: Test
testEvalEqual2 = TestCase $ do
  assertEqual "Eval Test Equal #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp Equal a2 b2
    result2 = Var (AstBool "#f")

testEvalEqual3 :: Test
testEvalEqual3 = TestCase $ do
  assertEqual "Eval Test Equal #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-3))
    b3 = Var (AstInt 7)
    operation3 = BinaryOp Equal a3 b3
    result3 = Var (AstBool "#f")

testEvalEqual4 :: Test
testEvalEqual4 = TestCase $ do
  assertEqual "Eval Test Equal #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-50))
    operation4 = BinaryOp Equal a4 b4
    result4 = Var (AstBool "#f")

testEvalEqual5 :: Test
testEvalEqual5 = TestCase $ do
  assertEqual "Eval Test Equal #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 1)
    operation5 = BinaryOp Equal a5 b5
    result5 = Var (AstBool "#f")

testEvalEqual6 :: Test
testEvalEqual6 = TestCase $ do
  assertEqual "Eval Test Equal #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 1)
    b6 = Var (AstInt 1)
    operation6 = BinaryOp Equal a6 b6
    result6 = Var (AstBool "#t")

testEvalEqual7 :: Test
testEvalEqual7 = TestCase $ do
  assertEqual "Eval Test Equal #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 0)
    operation7 = BinaryOp Equal a7 b7
    result7 = Var (AstBool "#f")

testEvalEqual8 :: Test
testEvalEqual8 = TestCase $ do
  assertEqual "Eval Test Equal #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 10)
    b8 = Var (AstInt 20)
    operation8 = BinaryOp Equal a8 b8
    result8 = Var (AstBool "#f")

testEvalEqual9 :: Test
testEvalEqual9 = TestCase $ do
  assertEqual "Eval Test Equal #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-2))
    b9 = Var (AstInt 2)
    operation9 = BinaryOp Equal a9 b9
    result9 = Var (AstBool "#f")

testEvalEqual10 :: Test
testEvalEqual10 = TestCase $ do
  assertEqual "Eval Test Equal #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstInt 0)
    operation10 = BinaryOp Equal a10 b10
    result10 = Var (AstBool "#t")
