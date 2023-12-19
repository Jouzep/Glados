module EvaluationTests.SimpleTests.EvaluationSimpleNotEqual (evalSimpleNotEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleNotEqualTests :: Test
evalSimpleNotEqualTests = TestList [testEvalNotEqual1, testEvalNotEqual2, testEvalNotEqual3, testEvalNotEqual4, testEvalNotEqual5, testEvalNotEqual6, testEvalNotEqual7, testEvalNotEqual8, testEvalNotEqual9, testEvalNotEqual10, testEvalNotEqual11]

testEvalNotEqual1 :: Test
testEvalNotEqual1 = TestCase $ do
  assertEqual "Eval Test NotEqual #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 2)
    operation1 = BinaryOp NotEqual a1 b1
    result1 = Var (AstBool "#t")

testEvalNotEqual2 :: Test
testEvalNotEqual2 = TestCase $ do
  assertEqual "Eval Test NotEqual #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 8)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp NotEqual a2 b2
    result2 = Var (AstBool "#f")

testEvalNotEqual3 :: Test
testEvalNotEqual3 = TestCase $ do
  assertEqual "Eval Test NotEqual #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-10))
    b3 = Var (AstInt 4)
    operation3 = BinaryOp NotEqual a3 b3
    result3 = Var (AstBool "#t")

testEvalNotEqual4 :: Test
testEvalNotEqual4 = TestCase $ do
  assertEqual "Eval Test NotEqual #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-100))
    operation4 = BinaryOp NotEqual a4 b4
    result4 = Var (AstBool "#t")

testEvalNotEqual5 :: Test
testEvalNotEqual5 = TestCase $ do
  assertEqual "Eval Test NotEqual #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 0)
    operation5 = BinaryOp NotEqual a5 b5
    result5 = Var (AstBool "#f")

testEvalNotEqual6 :: Test
testEvalNotEqual6 = TestCase $ do
  assertEqual "Eval Test NotEqual #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 15)
    b6 = Var (AstInt 15)
    operation6 = BinaryOp NotEqual a6 b6
    result6 = Var (AstBool "#f")

testEvalNotEqual7 :: Test
testEvalNotEqual7 = TestCase $ do
  assertEqual "Eval Test NotEqual #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 3)
    operation7 = BinaryOp NotEqual a7 b7
    result7 = Var (AstBool "#t")

testEvalNotEqual8 :: Test
testEvalNotEqual8 = TestCase $ do
  assertEqual "Eval Test NotEqual #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 23)
    b8 = Var (AstInt 9)
    operation8 = BinaryOp NotEqual a8 b8
    result8 = Var (AstBool "#t")

testEvalNotEqual9 :: Test
testEvalNotEqual9 = TestCase $ do
  assertEqual "Eval Test NotEqual #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-15))
    b9 = Var (AstInt 7)
    operation9 = BinaryOp NotEqual a9 b9
    result9 = Var (AstBool "#t")

testEvalNotEqual10 :: Test
testEvalNotEqual10 = TestCase $ do
  assertEqual "Eval Test NotEqual #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 30)
    b10 = Var (AstInt 11)
    operation10 = BinaryOp NotEqual a10 b10
    result10 = Var (AstBool "#t")

testEvalNotEqual11 :: Test
testEvalNotEqual11 = TestCase $ do
  assertEqual "Eval Test NotEqual #11" (result10) (evaluation operation10)
  where
    a10 = Var (AstInt 30)
    b10 = Var (AstSymb "bad input")
    operation10 = BinaryOp NotEqual a10 b10
    result10 = Nothing
