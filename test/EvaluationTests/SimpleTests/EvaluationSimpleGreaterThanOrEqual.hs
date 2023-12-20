module EvaluationTests.SimpleTests.EvaluationSimpleGreaterThanOrEqual (evalSimpleGreaterThanOrEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleGreaterThanOrEqualTests :: Test
evalSimpleGreaterThanOrEqualTests = TestList [testEvalGreaterThanOrEqual1, testEvalGreaterThanOrEqual2, testEvalGreaterThanOrEqual3, testEvalGreaterThanOrEqual4, testEvalGreaterThanOrEqual5, testEvalGreaterThanOrEqual6, testEvalGreaterThanOrEqual7, testEvalGreaterThanOrEqual8, testEvalGreaterThanOrEqual9, testEvalGreaterThanOrEqual10, testEvalGreaterThanOrEqual11]

testEvalGreaterThanOrEqual1 :: Test
testEvalGreaterThanOrEqual1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalGreaterThanOrEqual2 :: Test
testEvalGreaterThanOrEqual2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #2" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 8)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#f")

testEvalGreaterThanOrEqual3 :: Test
testEvalGreaterThanOrEqual3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #3" (Just expected) (result)
  where
    a = Var (AstInt (-3))
    b = Var (AstInt 7)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#f")

testEvalGreaterThanOrEqual4 :: Test
testEvalGreaterThanOrEqual4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-50))
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalGreaterThanOrEqual5 :: Test
testEvalGreaterThanOrEqual5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalGreaterThanOrEqual6 :: Test
testEvalGreaterThanOrEqual6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #6" (Just expected) (result)
  where
    a = Var (AstInt 1)
    b = Var (AstInt 1)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalGreaterThanOrEqual7 :: Test
testEvalGreaterThanOrEqual7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 0)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#f")

testEvalGreaterThanOrEqual8 :: Test
testEvalGreaterThanOrEqual8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #8" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 20)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#f")

testEvalGreaterThanOrEqual9 :: Test
testEvalGreaterThanOrEqual9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #9" (Just expected) (result)
  where
    a = Var (AstInt (-2))
    b = Var (AstInt 2)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#f")

testEvalGreaterThanOrEqual10 :: Test
testEvalGreaterThanOrEqual10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #10" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalGreaterThanOrEqual11 :: Test
testEvalGreaterThanOrEqual11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThanOrEqual #11" (expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstSymb "1")
    operation = BinaryOp GreaterThanOrEqual a b
    expected = Nothing