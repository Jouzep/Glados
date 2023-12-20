module EvaluationTests.SimpleTests.EvaluationSimpleLessThan (evalSimpleLessThanTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleLessThanTests :: Test
evalSimpleLessThanTests = TestList [testEvalLessThan1, testEvalLessThan2, testEvalLessThan3, testEvalLessThan4, testEvalLessThan5, testEvalLessThan6, testEvalLessThan7, testEvalLessThan8, testEvalLessThan9, testEvalLessThan10, testEvalLessThan11]

testEvalLessThan1 :: Test
testEvalLessThan1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#f")

testEvalLessThan2 :: Test
testEvalLessThan2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #2" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 8)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#t")

testEvalLessThan3 :: Test
testEvalLessThan3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #3" (Just expected) (result)
  where
    a = Var (AstInt (-3))
    b = Var (AstInt 7)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#t")

testEvalLessThan4 :: Test
testEvalLessThan4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-50))
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#f")

testEvalLessThan5 :: Test
testEvalLessThan5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#f")

testEvalLessThan6 :: Test
testEvalLessThan6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #6" (Just expected) (result)
  where
    a = Var (AstInt 1)
    b = Var (AstInt 1)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#f")

testEvalLessThan7 :: Test
testEvalLessThan7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 0)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#t")

testEvalLessThan8 :: Test
testEvalLessThan8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #8" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 20)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#t")

testEvalLessThan9 :: Test
testEvalLessThan9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #9" (Just expected) (result)
  where
    a = Var (AstInt (-2))
    b = Var (AstInt 2)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#t")

testEvalLessThan10 :: Test
testEvalLessThan10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #10" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = BinaryOp LessThan a b
    expected = Var (AstBool "#f")

testEvalLessThan11 :: Test
testEvalLessThan11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test LessThan #11" (expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstSymb "bad input")
    operation = BinaryOp LessThan a b
    expected = Nothing
