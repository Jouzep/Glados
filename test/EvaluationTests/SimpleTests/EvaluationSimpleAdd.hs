module EvaluationTests.SimpleTests.EvaluationSimpleAdd (evalSimpleAddTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env
evalSimpleAddTests :: Test
evalSimpleAddTests = TestList [testEvalAdd1, testEvalAdd2, testEvalAdd3, testEvalAdd4, testEvalAdd5, testEvalAdd6, testEvalAdd7, testEvalAdd8, testEvalAdd9, testEvalAdd10, testEvalAdd11]

testEvalAdd1 :: Test
testEvalAdd1 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #1" (Just expected) (result)
  where
    a = Var (AstInt 5)
    b = Var (AstInt 5)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 10)

testEvalAdd2 :: Test
testEvalAdd2 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #2" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 8)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 8)

testEvalAdd3 :: Test
testEvalAdd3 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #3" (Just expected) (result)
  where
    a = Var (AstInt (-3))
    b = Var (AstInt 7)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 4)

testEvalAdd4 :: Test
testEvalAdd4 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #4" (Just expected) (result)
  where
    a = Var (AstInt 100)
    b = Var (AstInt (-50))
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 50)

testEvalAdd5 :: Test
testEvalAdd5 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #5" (Just expected) (result)
  where
    a = Var (AstInt 0)
    b = Var (AstInt 0)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 0)

testEvalAdd6 :: Test
testEvalAdd6 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #6" (Just expected) (result)
  where
    a = Var (AstInt 25)
    b = Var (AstInt 5)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 30)

testEvalAdd7 :: Test
testEvalAdd7 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #7" (Just expected) (result)
  where
    a = Var (AstInt (-30))
    b = Var (AstInt 10)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt (-20))

testEvalAdd8 :: Test
testEvalAdd8 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #8" (Just expected) (result)
  where
    a = Var (AstInt 64)
    b = Var (AstInt 8)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 72)

testEvalAdd9 :: Test
testEvalAdd9 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #9" (Just expected) (result)
  where
    a = Var (AstInt 20)
    b = Var (AstInt (-2))
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 18)

testEvalAdd10 :: Test
testEvalAdd10 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #10" (Just expected) (result)
  where
    a = Var (AstInt 7)
    b = Var (AstInt 1)
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Var (AstInt 8)

testEvalAdd11 :: Test
testEvalAdd11 = TestCase $ do
  let (result, _) = evaluation operation createEnv
  assertEqual "Eval Test Add #11" (expected) (result)
  where
    a = Var (AstInt 7)
    b = Var (AstSymb "4")
    operation = ListOfAst [BinaryOp Add, a, b]
    expected = Nothing