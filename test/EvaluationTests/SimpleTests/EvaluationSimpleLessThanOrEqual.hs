module EvaluationTests.SimpleTests.EvaluationSimpleLessThanOrEqual (evalSimpleLessThanOrEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleLessThanOrEqualTests :: Test
evalSimpleLessThanOrEqualTests = TestList [testEvalLessThanOrEqual1, testEvalLessThanOrEqual2, testEvalLessThanOrEqual3, testEvalLessThanOrEqual4, testEvalLessThanOrEqual5, testEvalLessThanOrEqual6, testEvalLessThanOrEqual7, testEvalLessThanOrEqual8, testEvalLessThanOrEqual9, testEvalLessThanOrEqual10]

testEvalLessThanOrEqual1 :: Test
testEvalLessThanOrEqual1 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    operation1 = BinaryOp LessThanOrEqual a1 b1
    result1 = Var (AstBool "#t")

testEvalLessThanOrEqual2 :: Test
testEvalLessThanOrEqual2 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp LessThanOrEqual a2 b2
    result2 = Var (AstBool "#t")

testEvalLessThanOrEqual3 :: Test
testEvalLessThanOrEqual3 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-3))
    b3 = Var (AstInt 7)
    operation3 = BinaryOp LessThanOrEqual a3 b3
    result3 = Var (AstBool "#t")

testEvalLessThanOrEqual4 :: Test
testEvalLessThanOrEqual4 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-50))
    operation4 = BinaryOp LessThanOrEqual a4 b4
    result4 = Var (AstBool "#f")

testEvalLessThanOrEqual5 :: Test
testEvalLessThanOrEqual5 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 0)
    operation5 = BinaryOp LessThanOrEqual a5 b5
    result5 = Var (AstBool "#t")

testEvalLessThanOrEqual6 :: Test
testEvalLessThanOrEqual6 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 1)
    b6 = Var (AstInt 1)
    operation6 = BinaryOp LessThanOrEqual a6 b6
    result6 = Var (AstBool "#t")

testEvalLessThanOrEqual7 :: Test
testEvalLessThanOrEqual7 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 0)
    operation7 = BinaryOp LessThanOrEqual a7 b7
    result7 = Var (AstBool "#t")

testEvalLessThanOrEqual8 :: Test
testEvalLessThanOrEqual8 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 10)
    b8 = Var (AstInt 20)
    operation8 = BinaryOp LessThanOrEqual a8 b8
    result8 = Var (AstBool "#t")

testEvalLessThanOrEqual9 :: Test
testEvalLessThanOrEqual9 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-2))
    b9 = Var (AstInt 2)
    operation9 = BinaryOp LessThanOrEqual a9 b9
    result9 = Var (AstBool "#t")

testEvalLessThanOrEqual10 :: Test
testEvalLessThanOrEqual10 = TestCase $ do
  assertEqual "Eval Test LessThanOrEqual #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstInt 0)
    operation10 = BinaryOp LessThanOrEqual a10 b10
    result10 = Var (AstBool "#t")
