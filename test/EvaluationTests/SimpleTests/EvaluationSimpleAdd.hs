module EvaluationTests.SimpleTests.EvaluationSimpleAdd (evalSimpleAddTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleAddTests :: Test
evalSimpleAddTests = TestList [testEvalAdd1, testEvalAdd2, testEvalAdd3, testEvalAdd4, testEvalAdd5, testEvalAdd6, testEvalAdd7, testEvalAdd8, testEvalAdd9, testEvalAdd10]

testEvalAdd1 :: Test
testEvalAdd1 = TestCase $ do
  assertEqual "Eval Test Add #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 5)
    operation1 = BinaryOp Add a1 b1
    result1 = Var (AstInt 10)

testEvalAdd2 :: Test
testEvalAdd2 = TestCase $ do
  assertEqual "Eval Test Add #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 0)
    b2 = Var (AstInt 8)
    operation2 = BinaryOp Add a2 b2
    result2 = Var (AstInt 8)

testEvalAdd3 :: Test
testEvalAdd3 = TestCase $ do
  assertEqual "Eval Test Add #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-3))
    b3 = Var (AstInt 7)
    operation3 = BinaryOp Add a3 b3
    result3 = Var (AstInt 4)

testEvalAdd4 :: Test
testEvalAdd4 = TestCase $ do
  assertEqual "Eval Test Add #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-50))
    operation4 = BinaryOp Add a4 b4
    result4 = Var (AstInt 50)

testEvalAdd5 :: Test
testEvalAdd5 = TestCase $ do
  assertEqual "Eval Test Add #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 0)
    operation5 = BinaryOp Add a5 b5
    result5 = Var (AstInt 0)

testEvalAdd6 :: Test
testEvalAdd6 = TestCase $ do
  assertEqual "Eval Test Add #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 25)
    b6 = Var (AstInt 5)
    operation6 = BinaryOp Add a6 b6
    result6 = Var (AstInt 30)

testEvalAdd7 :: Test
testEvalAdd7 = TestCase $ do
  assertEqual "Eval Test Add #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-30))
    b7 = Var (AstInt 10)
    operation7 = BinaryOp Add a7 b7
    result7 = Var (AstInt (-20))

testEvalAdd8 :: Test
testEvalAdd8 = TestCase $ do
  assertEqual "Eval Test Add #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 64)
    b8 = Var (AstInt 8)
    operation8 = BinaryOp Add a8 b8
    result8 = Var (AstInt 72)

testEvalAdd9 :: Test
testEvalAdd9 = TestCase $ do
  assertEqual "Eval Test Add #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt 20)
    b9 = Var (AstInt (-2))
    operation9 = BinaryOp Add a9 b9
    result9 = Var (AstInt 18)

testEvalAdd10 :: Test
testEvalAdd10 = TestCase $ do
  assertEqual "Eval Test Add #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 7)
    b10 = Var (AstInt 1)
    operation10 = BinaryOp Add a10 b10
    result10 = Var (AstInt 8)
