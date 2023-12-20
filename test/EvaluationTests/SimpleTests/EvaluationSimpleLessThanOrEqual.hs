module EvaluationTests.SimpleTests.EvaluationSimpleLessThanOrEqual (evalSimpleLessThanOrEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleLessThanOrEqualTests :: Test
evalSimpleLessThanOrEqualTests = TestList [testEvalLessThanOrEqual1, testEvalLessThanOrEqual2, testEvalLessThanOrEqual3, testEvalLessThanOrEqual4, testEvalLessThanOrEqual5, testEvalLessThanOrEqual6, testEvalLessThanOrEqual7, testEvalLessThanOrEqual8, testEvalLessThanOrEqual9, testEvalLessThanOrEqual10, testEvalLessThanOrEqual11]

testEvalLessThanOrEqual1 :: Test
testEvalLessThanOrEqual1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual2 :: Test
testEvalLessThanOrEqual2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #2" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 8)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual3 :: Test
testEvalLessThanOrEqual3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #3" (Just expected) (result)
  where
    a = Var (AstInt (-3))
    b = Var (AstInt 7)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual4 :: Test
testEvalLessThanOrEqual4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-50))
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#f")

testEvalLessThanOrEqual5 :: Test
testEvalLessThanOrEqual5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual6 :: Test
testEvalLessThanOrEqual6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #6" (Just expected) (result)
  where
    a = Var (AstInt 1)
    b = Var (AstInt 1)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual7 :: Test
testEvalLessThanOrEqual7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 0)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual8 :: Test
testEvalLessThanOrEqual8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #8" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 20)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual9 :: Test
testEvalLessThanOrEqual9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #9" (Just expected) (result)
  where
    a = Var (AstInt (-2))
    b = Var (AstInt 2)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual10 :: Test
testEvalLessThanOrEqual10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #10" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = BinaryOp LessThanOrEqual a b
    expected = Var (AstBool "#t")

testEvalLessThanOrEqual11 :: Test
testEvalLessThanOrEqual11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThanOrEqual #11" (expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstSymb "bad input")
    operation = BinaryOp LessThanOrEqual a b
    expected = Nothing
