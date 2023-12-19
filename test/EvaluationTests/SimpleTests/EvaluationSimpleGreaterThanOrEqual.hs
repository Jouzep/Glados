module EvaluationTests.SimpleTests.EvaluationSimpleGreaterThanOrEqual (evalSimpleGreaterThanOrEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleGreaterThanOrEqualTests :: Test
evalSimpleGreaterThanOrEqualTests = TestList [testEvalGreaterThanOrEqual1, testEvalGreaterThanOrEqual2, testEvalGreaterThanOrEqual3, testEvalGreaterThanOrEqual4, testEvalGreaterThanOrEqual5, testEvalGreaterThanOrEqual6, testEvalGreaterThanOrEqual7, testEvalGreaterThanOrEqual8, testEvalGreaterThanOrEqual9, testEvalGreaterThanOrEqual10, testEvalGreaterThanOrEqual11]

testEvalGreaterThanOrEqual1 :: Test
testEvalGreaterThanOrEqual1 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    operation1 = BinaryOp GreaterThanOrEqual a1 b1
    result1 = Var (AstBool "#t")

testEvalGreaterThanOrEqual2 :: Test
testEvalGreaterThanOrEqual2 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp GreaterThanOrEqual a2 b2
    result2 = Var (AstBool "#f")

testEvalGreaterThanOrEqual3 :: Test
testEvalGreaterThanOrEqual3 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-3))
    b3 = Var (AstInt 7)
    operation3 = BinaryOp GreaterThanOrEqual a3 b3
    result3 = Var (AstBool "#f")

testEvalGreaterThanOrEqual4 :: Test
testEvalGreaterThanOrEqual4 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-50))
    operation4 = BinaryOp GreaterThanOrEqual a4 b4
    result4 = Var (AstBool "#t")

testEvalGreaterThanOrEqual5 :: Test
testEvalGreaterThanOrEqual5 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 0)
    operation5 = BinaryOp GreaterThanOrEqual a5 b5
    result5 = Var (AstBool "#t")

testEvalGreaterThanOrEqual6 :: Test
testEvalGreaterThanOrEqual6 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 1)
    b6 = Var (AstInt 1)
    operation6 = BinaryOp GreaterThanOrEqual a6 b6
    result6 = Var (AstBool "#t")

testEvalGreaterThanOrEqual7 :: Test
testEvalGreaterThanOrEqual7 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 0)
    operation7 = BinaryOp GreaterThanOrEqual a7 b7
    result7 = Var (AstBool "#f")

testEvalGreaterThanOrEqual8 :: Test
testEvalGreaterThanOrEqual8 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 10)
    b8 = Var (AstInt 20)
    operation8 = BinaryOp GreaterThanOrEqual a8 b8
    result8 = Var (AstBool "#f")

testEvalGreaterThanOrEqual9 :: Test
testEvalGreaterThanOrEqual9 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-2))
    b9 = Var (AstInt 2)
    operation9 = BinaryOp GreaterThanOrEqual a9 b9
    result9 = Var (AstBool "#f")

testEvalGreaterThanOrEqual10 :: Test
testEvalGreaterThanOrEqual10 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstInt 0)
    operation10 = BinaryOp GreaterThanOrEqual a10 b10
    result10 = Var (AstBool "#t")

testEvalGreaterThanOrEqual11 :: Test
testEvalGreaterThanOrEqual11 = TestCase $ do
  assertEqual "Eval Test GreaterThanOrEqual #11" (result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstSymb "1")
    operation10 = BinaryOp GreaterThanOrEqual a10 b10
    result10 = Nothing