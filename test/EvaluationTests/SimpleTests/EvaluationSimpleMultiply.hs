module EvaluationTests.SimpleTests.EvaluationSimpleMultiply (evalSimpleMultiplyTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleMultiplyTests :: Test
evalSimpleMultiplyTests = TestList [testEvalMultiply1, testEvalMultiply2, testEvalMultiply3, testEvalMultiply4, testEvalMultiply5, testEvalMultiply6, testEvalMultiply7, testEvalMultiply8, testEvalMultiply9, testEvalMultiply10]

testEvalMultiply1 :: Test
testEvalMultiply1 = TestCase $ do
  assertEqual "Eval Test Multiply #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 2)
    operation1 = BinaryOp Multiply a1 b1
    result1 = Var (AstInt 10)

testEvalMultiply2 :: Test
testEvalMultiply2 = TestCase $ do
  assertEqual "Eval Test Multiply #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 8)
    b2 = Var (AstInt 3)
    operation2 = BinaryOp Multiply a2 b2
    result2 = Var (AstInt 24)

testEvalMultiply3 :: Test
testEvalMultiply3 = TestCase $ do
  assertEqual "Eval Test Multiply #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-10))
    b3 = Var (AstInt 4)
    operation3 = BinaryOp Multiply a3 b3
    result3 = Var (AstInt (-40))

testEvalMultiply4 :: Test
testEvalMultiply4 = TestCase $ do
  assertEqual "Eval Test Multiply #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-30))
    operation4 = BinaryOp Multiply a4 b4
    result4 = Var (AstInt (-3000))

testEvalMultiply5 :: Test
testEvalMultiply5 = TestCase $ do
  assertEqual "Eval Test Multiply #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 5)
    operation5 = BinaryOp Multiply a5 b5
    result5 = Var (AstInt 0)

testEvalMultiply6 :: Test
testEvalMultiply6 = TestCase $ do
  assertEqual "Eval Test Multiply #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 15)
    b6 = Var (AstInt 7)
    operation6 = BinaryOp Multiply a6 b6
    result6 = Var (AstInt 105)

testEvalMultiply7 :: Test
testEvalMultiply7 = TestCase $ do
  assertEqual "Eval Test Multiply #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 3)
    operation7 = BinaryOp Multiply a7 b7
    result7 = Var (AstInt (-15))

testEvalMultiply8 :: Test
testEvalMultiply8 = TestCase $ do
  assertEqual "Eval Test Multiply #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 23)
    b8 = Var (AstInt 9)
    operation8 = BinaryOp Multiply a8 b8
    result8 = Var (AstInt 207)

testEvalMultiply9 :: Test
testEvalMultiply9 = TestCase $ do
  assertEqual "Eval Test Multiply #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-15))
    b9 = Var (AstInt 7)
    operation9 = BinaryOp Multiply a9 b9
    result9 = Var (AstInt (-105))

testEvalMultiply10 :: Test
testEvalMultiply10 = TestCase $ do
  assertEqual "Eval Test Multiply #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 30)
    b10 = Var (AstInt 11)
    operation10 = BinaryOp Multiply a10 b10
    result10 = Var (AstInt 330)