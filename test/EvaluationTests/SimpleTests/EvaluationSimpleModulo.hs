module EvaluationTests.SimpleTests.EvaluationSimpleModulo (evalSimpleModuloTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalSimpleModuloTests :: Test
evalSimpleModuloTests = TestList [testEvalModulo1, testEvalModulo2, testEvalModulo3, testEvalModulo4, testEvalModulo5, testEvalModulo6, testEvalModulo7, testEvalModulo8, testEvalModulo9, testEvalModulo10]

testEvalModulo1 :: Test
testEvalModulo1 = TestCase $ do
  assertEqual "Eval Test Modulo #1" (Just result1) (evaluation operation1)
  where
    a1 = Var (AstInt 5)
    b1 = Var (AstInt 2)
    operation1 = BinaryOp Modulo a1 b1
    result1 = Var (AstInt 1)

testEvalModulo2 :: Test
testEvalModulo2 = TestCase $ do
  assertEqual "Eval Test Modulo #2" (Just result2) (evaluation operation2)
  where
    a2 = Var (AstInt 8)
    b2 = Var (AstInt 3)
    operation2 = BinaryOp Modulo a2 b2
    result2 = Var (AstInt 2)

testEvalModulo3 :: Test
testEvalModulo3 = TestCase $ do
  assertEqual "Eval Test Modulo #3" (Just result3) (evaluation operation3)
  where
    a3 = Var (AstInt (-10))
    b3 = Var (AstInt 4)
    operation3 = BinaryOp Modulo a3 b3
    result3 = Var (AstInt (-2))

testEvalModulo4 :: Test
testEvalModulo4 = TestCase $ do
  assertEqual "Eval Test Modulo #4" (Just result4) (evaluation operation4)
  where
    a4 = Var (AstInt 100)
    b4 = Var (AstInt (-30))
    operation4 = BinaryOp Modulo a4 b4
    result4 = Var (AstInt 10)

testEvalModulo5 :: Test
testEvalModulo5 = TestCase $ do
  assertEqual "Eval Test Modulo #5" (Just result5) (evaluation operation5)
  where
    a5 = Var (AstInt 0)
    b5 = Var (AstInt 5)
    operation5 = BinaryOp Modulo a5 b5
    result5 = Var (AstInt 0)

testEvalModulo6 :: Test
testEvalModulo6 = TestCase $ do
  assertEqual "Eval Test Modulo #6" (Just result6) (evaluation operation6)
  where
    a6 = Var (AstInt 15)
    b6 = Var (AstInt 7)
    operation6 = BinaryOp Modulo a6 b6
    result6 = Var (AstInt 1)

testEvalModulo7 :: Test
testEvalModulo7 = TestCase $ do
  assertEqual "Eval Test Modulo #7" (Just result7) (evaluation operation7)
  where
    a7 = Var (AstInt (-5))
    b7 = Var (AstInt 3)
    operation7 = BinaryOp Modulo a7 b7
    result7 = Var (AstInt (-2))

testEvalModulo8 :: Test
testEvalModulo8 = TestCase $ do
  assertEqual "Eval Test Modulo #8" (Just result8) (evaluation operation8)
  where
    a8 = Var (AstInt 23)
    b8 = Var (AstInt 9)
    operation8 = BinaryOp Modulo a8 b8
    result8 = Var (AstInt 5)

testEvalModulo9 :: Test
testEvalModulo9 = TestCase $ do
  assertEqual "Eval Test Modulo #9" (Just result9) (evaluation operation9)
  where
    a9 = Var (AstInt (-15))
    b9 = Var (AstInt 7)
    operation9 = BinaryOp Modulo a9 b9
    result9 = Var (AstInt (-1))

testEvalModulo10 :: Test
testEvalModulo10 = TestCase $ do
  assertEqual "Eval Test Modulo #10" (Just result10) (evaluation operation10)
  where
    a10 = Var (AstInt 30)
    b10 = Var (AstInt 11)
    operation10 = BinaryOp Modulo a10 b10
    result10 = Var (AstInt 8)