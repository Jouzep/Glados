module EvaluationTests.SimpleTests.EvaluationSimpleSub (evalSimpleSubTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleSubTests :: Test
evalSimpleSubTests = TestList[ testEvalSub1, testEvalSub2, testEvalSub3, testEvalSub4, testEvalSub5, testEvalSub6, testEvalSub7, testEvalSub8, testEvalSub9, testEvalSub10, testEvalSub11]

testEvalSub1 :: Test
testEvalSub1 = TestCase $ do
  assertEqual "Eval Test Sub #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 3)
    operation1 = BinaryOp Sub a1 b1
    result1 = Var (AstInt 2)

testEvalSub2 :: Test
testEvalSub2 = TestCase $ do
  assertEqual "Eval Test Sub #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp Sub a2 b2
    result2 = Var (AstInt (-8))

testEvalSub3 :: Test
testEvalSub3 = TestCase $ do
  assertEqual "Eval Test Sub #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-3))
    b3 = Var (AstInt 7)
    operation3 = BinaryOp Sub a3 b3
    result3 = Var (AstInt (-10))

testEvalSub4 :: Test
testEvalSub4 = TestCase $ do
  assertEqual "Eval Test Sub #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-50))
    operation4 = BinaryOp Sub a4 b4
    result4 = Var (AstInt 150)

testEvalSub5 :: Test
testEvalSub5 = TestCase $ do
  assertEqual "Eval Test Sub #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 0)
    operation5 = BinaryOp Sub a5 b5
    result5 = Var (AstInt 0)

testEvalSub6 :: Test
testEvalSub6 = TestCase $ do
  assertEqual "Eval Test Sub #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 8)
    b6 = Var (AstInt 3)
    operation6 = BinaryOp Sub a6 b6
    result6 = Var (AstInt 5)

testEvalSub7 :: Test
testEvalSub7 = TestCase $ do
  assertEqual "Eval Test Sub #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt 15)
    b7 = Var (AstInt 10)
    operation7 = BinaryOp Sub a7 b7
    result7 = Var (AstInt 5)

testEvalSub8 :: Test
testEvalSub8 = TestCase $ do
  assertEqual "Eval Test Sub #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt (-5))
    b8 = Var (AstInt (-8))
    operation8 = BinaryOp Sub a8 b8
    result8 = Var (AstInt 3)

testEvalSub9 :: Test
testEvalSub9 = TestCase $ do
  assertEqual "Eval Test Sub #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt 20)
    b9 = Var (AstInt 5)
    operation9 = BinaryOp Sub a9 b9
    result9 = Var (AstInt 15)

testEvalSub10 :: Test
testEvalSub10 = TestCase $ do
  assertEqual "Eval Test Sub #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 7)
    b10 = Var (AstInt 2)
    operation10 = BinaryOp Sub a10 b10
    result10 = Var (AstInt 5)

testEvalSub11 :: Test
testEvalSub11 = TestCase $ do
  assertEqual "Eval Test Sub #11" ( result10) (evaluation operation10)
  where
    a10 = Var (AstInt 7)
    b10 = Var (AstSymb "bad input")
    operation10 = BinaryOp Sub a10 b10
    result10 = Nothing