module EvaluationTests.SimpleTests.EvaluationSimpleGreaterThan (evalSimpleGreaterThanTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleGreaterThanTests :: Test
evalSimpleGreaterThanTests = TestList [testEvalGreaterThan1, testEvalGreaterThan2, testEvalGreaterThan3, testEvalGreaterThan4, testEvalGreaterThan5, testEvalGreaterThan6, testEvalGreaterThan7, testEvalGreaterThan8, testEvalGreaterThan9, testEvalGreaterThan10, testEvalGreaterThan11]

testEvalGreaterThan1 :: Test
testEvalGreaterThan1 = TestCase $ do
  assertEqual "Eval Test GreaterThan #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    operation1 = BinaryOp GreaterThan a1 b1
    result1 = Var (AstBool "#f")

testEvalGreaterThan2 :: Test
testEvalGreaterThan2 = TestCase $ do
  assertEqual "Eval Test GreaterThan #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp GreaterThan a2 b2
    result2 = Var (AstBool "#f")

testEvalGreaterThan3 :: Test
testEvalGreaterThan3 = TestCase $ do
  assertEqual "Eval Test GreaterThan #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-3))
    b3 = Var (AstInt 7)
    operation3 = BinaryOp GreaterThan a3 b3
    result3 = Var (AstBool "#f")

testEvalGreaterThan4 :: Test
testEvalGreaterThan4 = TestCase $ do
  assertEqual "Eval Test GreaterThan #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-50))
    operation4 = BinaryOp GreaterThan a4 b4
    result4 = Var (AstBool "#t")

testEvalGreaterThan5 :: Test
testEvalGreaterThan5 = TestCase $ do
  assertEqual "Eval Test GreaterThan #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 1)
    operation5 = BinaryOp GreaterThan a5 b5
    result5 = Var (AstBool "#f")

testEvalGreaterThan6 :: Test
testEvalGreaterThan6 = TestCase $ do
  assertEqual "Eval Test GreaterThan #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 1)
    b6 = Var (AstInt 1)
    operation6 = BinaryOp GreaterThan a6 b6
    result6 = Var (AstBool "#f")

testEvalGreaterThan7 :: Test
testEvalGreaterThan7 = TestCase $ do
  assertEqual "Eval Test GreaterThan #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 0)
    operation7 = BinaryOp GreaterThan a7 b7
    result7 = Var (AstBool "#f")

testEvalGreaterThan8 :: Test
testEvalGreaterThan8 = TestCase $ do
  assertEqual "Eval Test GreaterThan #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 10)
    b8 = Var (AstInt 20)
    operation8 = BinaryOp GreaterThan a8 b8
    result8 = Var (AstBool "#f")

testEvalGreaterThan9 :: Test
testEvalGreaterThan9 = TestCase $ do
  assertEqual "Eval Test GreaterThan #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-2))
    b9 = Var (AstInt 2)
    operation9 = BinaryOp GreaterThan a9 b9
    result9 = Var (AstBool "#f")

testEvalGreaterThan10 :: Test
testEvalGreaterThan10 = TestCase $ do
  assertEqual "Eval Test GreaterThan #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstInt 0)
    operation10 = BinaryOp GreaterThan a10 b10
    result10 = Var (AstBool "#f")

testEvalGreaterThan11 :: Test
testEvalGreaterThan11 = TestCase $ do
  assertEqual "Eval Test GreaterThan #11" (result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstSymb "15")
    operation10 = BinaryOp GreaterThan a10 b10
    result10 = Nothing