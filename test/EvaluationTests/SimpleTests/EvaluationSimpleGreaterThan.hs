module EvaluationTests.SimpleTests.EvaluationSimpleGreaterThan (evalSimpleGreaterThanTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleGreaterThanTests :: Test
evalSimpleGreaterThanTests = TestList [testEvalGreaterThan1, testEvalGreaterThan2, testEvalGreaterThan3, testEvalGreaterThan4, testEvalGreaterThan5, testEvalGreaterThan6, testEvalGreaterThan7, testEvalGreaterThan8, testEvalGreaterThan9, testEvalGreaterThan10, testEvalGreaterThan11]

testEvalGreaterThan1 :: Test
testEvalGreaterThan1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan2 :: Test
testEvalGreaterThan2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #2" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 8)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan3 :: Test
testEvalGreaterThan3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #3" (Just expected) (result)
  where
    a = Var (AstInt (-3))
    b = Var (AstInt 7)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan4 :: Test
testEvalGreaterThan4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-50))
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#t")

testEvalGreaterThan5 :: Test
testEvalGreaterThan5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 1)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan6 :: Test
testEvalGreaterThan6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #6" (Just expected) (result)
  where
    a = Var (AstInt 1)
    b = Var (AstInt 1)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan7 :: Test
testEvalGreaterThan7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 0)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan8 :: Test
testEvalGreaterThan8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #8" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 20)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan9 :: Test
testEvalGreaterThan9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #9" (Just expected) (result)
  where
    a = Var (AstInt (-2))
    b = Var (AstInt 2)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan10 :: Test
testEvalGreaterThan10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #10" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Var (AstBool "#f")

testEvalGreaterThan11 :: Test
testEvalGreaterThan11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test GreaterThan #11" (expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstSymb "15")
    operation = ListOfAst [BinaryOp GreaterThan, a, b]
    expected = Nothing