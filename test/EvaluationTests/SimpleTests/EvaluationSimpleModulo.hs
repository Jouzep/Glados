module EvaluationTests.SimpleTests.EvaluationSimpleModulo (evalSimpleModuloTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleModuloTests :: Test
evalSimpleModuloTests = TestList [testEvalModulo1, testEvalModulo2, testEvalModulo3, testEvalModulo4, testEvalModulo5, testEvalModulo6, testEvalModulo7, testEvalModulo8, testEvalModulo9, testEvalModulo10, testEvalModulo11, testEvalModulo12]

testEvalModulo1 :: Test
testEvalModulo1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 2)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt 1)

testEvalModulo2 :: Test
testEvalModulo2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #2" (Just expected) (result)
  where
    a = Var (AstInt 8)
    b = Var (AstInt 3)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt 2)

testEvalModulo3 :: Test
testEvalModulo3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #3" (Just expected) (result)
  where
    a = Var (AstInt (-10))
    b = Var (AstInt 4)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt (2))

testEvalModulo4 :: Test
testEvalModulo4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-30))
    operation = BinaryOp Modulo a b
    expected = Var (AstInt (-20))

testEvalModulo5 :: Test
testEvalModulo5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 5)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt 0)

testEvalModulo6 :: Test
testEvalModulo6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #6" (Just expected) (result)
  where
    a = Var (AstInt 15)
    b = Var (AstInt 7)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt 1)

testEvalModulo7 :: Test
testEvalModulo7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 3)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt (1))

testEvalModulo8 :: Test
testEvalModulo8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #8" (Just expected) (result)
  where
    a = Var (AstInt 23)
    b = Var (AstInt 9)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt 5)

testEvalModulo9 :: Test
testEvalModulo9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #9" (Just expected) (result)
  where
    a = Var (AstInt (-15))
    b = Var (AstInt 7)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt (6))

testEvalModulo10 :: Test
testEvalModulo10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #10" (Just expected) (result)
  where
    a = Var (AstInt 30)
    b = Var (AstInt 11)
    operation = BinaryOp Modulo a b
    expected = Var (AstInt 8)

testEvalModulo11 :: Test
testEvalModulo11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #11" (expected) (result)
  where
    a = Var (AstInt 30)
    b = Var (AstSymb "bad input")
    operation = BinaryOp Modulo a b
    expected = Nothing

-- Modulo by Zero
testEvalModulo12 :: Test
testEvalModulo12 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Modulo #12" (expected) (result)
  where
    a = Var (AstInt 30)
    b = Var (AstInt 0)
    operation = BinaryOp Modulo a b
    expected = Nothing