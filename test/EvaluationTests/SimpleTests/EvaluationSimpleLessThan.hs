module EvaluationTests.SimpleTests.EvaluationSimpleLessThan (evalSimpleLessThanTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleLessThanTests :: Test
evalSimpleLessThanTests = TestList [testEvalLessThan1, testEvalLessThan2, testEvalLessThan3, testEvalLessThan4, testEvalLessThan5, testEvalLessThan6, testEvalLessThan7, testEvalLessThan8, testEvalLessThan9, testEvalLessThan10, testEvalLessThan11]

testEvalLessThan1 :: Test
testEvalLessThan1 = TestCase $ do
  assertEqual "Eval Test LessThan #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    operation1 = BinaryOp LessThan a1 b1
    result1 = Var (AstBool "#f")

testEvalLessThan2 :: Test
testEvalLessThan2 = TestCase $ do
  assertEqual "Eval Test LessThan #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp LessThan a2 b2
    result2 = Var (AstBool "#t")

testEvalLessThan3 :: Test
testEvalLessThan3 = TestCase $ do
  assertEqual "Eval Test LessThan #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-3))
    b3 = Var (AstInt 7)
    operation3 = BinaryOp LessThan a3 b3
    result3 = Var (AstBool "#t")

testEvalLessThan4 :: Test
testEvalLessThan4 = TestCase $ do
  assertEqual "Eval Test LessThan #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-50))
    operation4 = BinaryOp LessThan a4 b4
    result4 = Var (AstBool "#f")

testEvalLessThan5 :: Test
testEvalLessThan5 = TestCase $ do
  assertEqual "Eval Test LessThan #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 0)
    operation5 = BinaryOp LessThan a5 b5
    result5 = Var (AstBool "#f")

testEvalLessThan6 :: Test
testEvalLessThan6 = TestCase $ do
  assertEqual "Eval Test LessThan #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 1)
    b6 = Var (AstInt 1)
    operation6 = BinaryOp LessThan a6 b6
    result6 = Var (AstBool "#f")

testEvalLessThan7 :: Test
testEvalLessThan7 = TestCase $ do
  assertEqual "Eval Test LessThan #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 0)
    operation7 = BinaryOp LessThan a7 b7
    result7 = Var (AstBool "#t")

testEvalLessThan8 :: Test
testEvalLessThan8 = TestCase $ do
  assertEqual "Eval Test LessThan #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 10)
    b8 = Var (AstInt 20)
    operation8 = BinaryOp LessThan a8 b8
    result8 = Var (AstBool "#t")

testEvalLessThan9 :: Test
testEvalLessThan9 = TestCase $ do
  assertEqual "Eval Test LessThan #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-2))
    b9 = Var (AstInt 2)
    operation9 = BinaryOp LessThan a9 b9
    result9 = Var (AstBool "#t")

testEvalLessThan10 :: Test
testEvalLessThan10 = TestCase $ do
  assertEqual "Eval Test LessThan #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstInt 0)
    operation10 = BinaryOp LessThan a10 b10
    result10 = Var (AstBool "#f")

testEvalLessThan11 :: Test
testEvalLessThan11 = TestCase $ do
  assertEqual "Eval Test LessThan #11" result10 (evaluation operation10)
  where
    a10 = Var (AstInt 0)
    b10 = Var (AstSymb "bad input")
    operation10 = BinaryOp LessThan a10 b10
    result10 = Nothing
