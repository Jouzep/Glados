module EvaluationTests.SimpleTests.EvaluationSimpleNotEqual (evalSimpleNotEqualTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleNotEqualTests :: Test
evalSimpleNotEqualTests = TestList [testEvalNotEqual1, testEvalNotEqual2, testEvalNotEqual3, testEvalNotEqual4, testEvalNotEqual5, testEvalNotEqual6, testEvalNotEqual7, testEvalNotEqual8, testEvalNotEqual9, testEvalNotEqual10, testEvalNotEqual11]

testEvalNotEqual1 :: Test
testEvalNotEqual1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 2)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#t")

testEvalNotEqual2 :: Test
testEvalNotEqual2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #2" (Just expected) (result)
  where
    a = Var (AstInt 8)
    b = Var (AstInt 8)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#f")

testEvalNotEqual3 :: Test
testEvalNotEqual3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #3" (Just expected) (result)
  where
    a = Var (AstInt (-10))
    b = Var (AstInt 4)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#t")

testEvalNotEqual4 :: Test
testEvalNotEqual4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-100))
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#t")

testEvalNotEqual5 :: Test
testEvalNotEqual5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#f")

testEvalNotEqual6 :: Test
testEvalNotEqual6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #6" (Just expected) (result)
  where
    a = Var (AstInt 15)
    b = Var (AstInt 15)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#f")

testEvalNotEqual7 :: Test
testEvalNotEqual7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #7" (Just expected) (result)
  where
    a = Var (AstInt (-5))
    b = Var (AstInt 3)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#t")

testEvalNotEqual8 :: Test
testEvalNotEqual8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #8" (Just expected) (result)
  where
    a = Var (AstInt 23)
    b = Var (AstInt 9)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#t")

testEvalNotEqual9 :: Test
testEvalNotEqual9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #9" (Just expected) (result)
  where
    a = Var (AstInt (-15))
    b = Var (AstInt 7)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#t")

testEvalNotEqual10 :: Test
testEvalNotEqual10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #10" (Just expected) (result)
  where
    a = Var (AstInt 30)
    b = Var (AstInt 11)
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Var (AstBool "#t")

testEvalNotEqual11 :: Test
testEvalNotEqual11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test NotEqual #11" (expected) (result)
  where
    a = Var (AstInt 30)
    b = Var (AstSymb "bad input")
    operation = ListOfAst [BinaryOp NotEqual, a, b]
    expected = Nothing
