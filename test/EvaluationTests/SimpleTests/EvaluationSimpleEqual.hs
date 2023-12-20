module EvaluationTests.SimpleTests.EvaluationSimpleEqual (evalSimpleEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleEqualTests :: Test
evalSimpleEqualTests = TestList [testEvalEqual1, testEvalEqual2, testEvalEqual3, testEvalEqual4, testEvalEqual5, testEvalEqual6, testEvalEqual7, testEvalEqual8, testEvalEqual9, testEvalEqual10, testEvalEqual11]

testEvalEqual1 :: Test
testEvalEqual1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#t")

testEvalEqual2 :: Test
testEvalEqual2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #2" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 8)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#f")

testEvalEqual3 :: Test
testEvalEqual3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #3" (Just expected) (result)
  where
    a = Var (AstInt (-3))
    b = Var (AstInt 7)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#f")

testEvalEqual4 :: Test
testEvalEqual4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-50))
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#f")

testEvalEqual5 :: Test
testEvalEqual5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 1)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#f")

testEvalEqual6 :: Test
testEvalEqual6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #6" (Just expected) (result)
  where
    a = Var (AstInt 1)
    b = Var (AstInt 1)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#t")

testEvalEqual7 :: Test
testEvalEqual7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 0)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#f")

testEvalEqual8 :: Test
testEvalEqual8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #8" (Just expected) (result)
  where
    a = Var (AstInt 10)
    b = Var (AstInt 20)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#f")

testEvalEqual9 :: Test
testEvalEqual9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #9" (Just expected) (result)
  where
    a = Var (AstInt (-2))
    b = Var (AstInt 2)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#f")

testEvalEqual10 :: Test
testEvalEqual10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #10" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Var (AstBool "#t")

testEvalEqual11 :: Test
testEvalEqual11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Equal #11" (expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstSymb "45")
    operation = ListOfAst [BinaryOp Equal, a, b]
    expected = Nothing
