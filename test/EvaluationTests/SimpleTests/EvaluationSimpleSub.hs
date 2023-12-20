module EvaluationTests.SimpleTests.EvaluationSimpleSub (evalSimpleSubTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env

evalSimpleSubTests :: Test
evalSimpleSubTests = TestList[ testEvalSub1, testEvalSub2, testEvalSub3, testEvalSub4, testEvalSub5, testEvalSub6, testEvalSub7, testEvalSub8, testEvalSub9, testEvalSub10, testEvalSub11]

testEvalSub1 :: Test
testEvalSub1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #1" (Just expected) result
  where
    a = Var (AstInt 5)
    b = Var (AstInt 3)
    operation = BinaryOp Sub a b
    expected = Var (AstInt 2)

testEvalSub2 :: Test
testEvalSub2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #2" (Just expected) result
  where
    a = Var (AstInt 0)
    b = Var (AstInt 8)
    operation = BinaryOp Sub a b
    expected = Var (AstInt (-8))

testEvalSub3 :: Test
testEvalSub3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #3" (Just expected) result
  where
    a = Var (AstInt (-3))
    b = Var (AstInt 7)
    operation = BinaryOp Sub a b
    expected = Var (AstInt (-10))

testEvalSub4 :: Test
testEvalSub4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #4" (Just expected) result
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-50))
    operation = BinaryOp Sub a b
    expected = Var (AstInt 150)

testEvalSub5 :: Test
testEvalSub5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #5" (Just expected) result
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = BinaryOp Sub a b
    expected = Var (AstInt 0)

testEvalSub6 :: Test
testEvalSub6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #6" (Just expected) result
  where
    a = Var (AstInt 8)
    b = Var (AstInt 3)
    operation = BinaryOp Sub a b
    expected = Var (AstInt 5)

testEvalSub7 :: Test
testEvalSub7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #7" (Just expected) result
  where
    a = Var (AstInt 15)
    b = Var (AstInt 10)
    operation = BinaryOp Sub a b
    expected = Var (AstInt 5)

testEvalSub8 :: Test
testEvalSub8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #8" (Just expected) result
  where
    a = Var (AstInt (-5))
    b = Var (AstInt (-8))
    operation = BinaryOp Sub a b
    expected = Var (AstInt 3)

testEvalSub9 :: Test
testEvalSub9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #9" (Just expected) result
  where
    a = Var (AstInt 20)
    b = Var (AstInt 5)
    operation = BinaryOp Sub a b
    expected = Var (AstInt 15)

testEvalSub10 :: Test
testEvalSub10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #10" (Just expected) result
  where
    a = Var (AstInt 7)
    b = Var (AstInt 2)
    operation = BinaryOp Sub a b
    expected = Var (AstInt 5)


testEvalSub11 :: Test
testEvalSub11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Sub #11" expected result
  where
    a = Var (AstInt 7)
    b = Var (AstSymb "bad input")
    operation = BinaryOp Sub a b
    expected = Nothing

