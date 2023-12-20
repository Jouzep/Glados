module EvaluationTests.SimpleTests.EvaluationSimpleMultiply (evalSimpleMultiplyTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleMultiplyTests :: Test
evalSimpleMultiplyTests = TestList [testEvalMultiply1, testEvalMultiply2, testEvalMultiply3, testEvalMultiply4, testEvalMultiply5, testEvalMultiply6, testEvalMultiply7, testEvalMultiply8, testEvalMultiply9, testEvalMultiply10, testEvalMultiply11]

testEvalMultiply1 :: Test
testEvalMultiply1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 2)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt 10)

testEvalMultiply2 :: Test
testEvalMultiply2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #2" (Just expected) (result)
  where
    a = Var (AstInt 8)
    b = Var (AstInt 3)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt 24)

testEvalMultiply3 :: Test
testEvalMultiply3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #3" (Just expected) (result)
  where
    a = Var (AstInt (-10))
    b = Var (AstInt 4)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt (-40))

testEvalMultiply4 :: Test
testEvalMultiply4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-30))
    operation = BinaryOp Multiply a b
    expected = Var (AstInt (-3000))

testEvalMultiply5 :: Test
testEvalMultiply5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 5)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt 0)

testEvalMultiply6 :: Test
testEvalMultiply6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #6" (Just expected) (result)
  where
    a = Var (AstInt 15)
    b = Var (AstInt 7)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt 105)

testEvalMultiply7 :: Test
testEvalMultiply7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 3)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt (-15))

testEvalMultiply8 :: Test
testEvalMultiply8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #8" (Just expected) (result)
  where
    a = Var (AstInt 23)
    b = Var (AstInt 9)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt 207)

testEvalMultiply9 :: Test
testEvalMultiply9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #9" (Just expected) (result)
  where
    a = Var (AstInt (-15))
    b = Var (AstInt 7)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt (-105))

testEvalMultiply10 :: Test
testEvalMultiply10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #10" (Just expected) (result)
  where
    a = Var (AstInt 30)
    b = Var (AstInt 11)
    operation = BinaryOp Multiply a b
    expected = Var (AstInt 330)

testEvalMultiply11 :: Test
testEvalMultiply11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Multiply #11" (expected) (result)
  where
    a = Var (AstInt 30)
    b = Var (AstSymb "4")
    operation = BinaryOp Multiply a b
    expected = Nothing