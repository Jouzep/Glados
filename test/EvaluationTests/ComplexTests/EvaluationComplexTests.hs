module EvaluationTests.ComplexTests.EvaluationComplexTests (evalComplexTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)
import AST.Env

evalComplexTests :: Test
evalComplexTests = TestList
    [testComplex1, testComplex2, testComplex3, testComplex4, testComplex5, testComplex6, testComplex7, testComplex8, testComplex9, testComplex10]

testComplex1 :: Test
testComplex1 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #1" (Just expected) (result)
    where
      a = BinaryOp Add (Var (AstInt 10)) (Var (AstInt 5))
      b = BinaryOp Multiply (Var (AstInt 2)) (Var (AstInt 3))
      operation = ListOfAst [BinaryOp Divide, a, b]
      expected = Var (AstInt 2) -- (10 + 5) / (2 * 3) = 15 / 6 = 2

testComplex2 :: Test
testComplex2 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #2" (Just expected) (result)
    where
      a = BinaryOp GreaterThan (Var (AstInt 15)) (Var (AstInt 10))
      b = BinaryOp LessThan (Var (AstInt 5)) (Var (AstInt 8))
      operation = ListOfAst [BinaryOp And, a, b]
      expected = Var (AstBool "#t") -- 15 > 10 AND 5 < 8

testComplex3 :: Test
testComplex3 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #3" (Just expected) (result)
    where
      a = BinaryOp Equal (Var (AstInt 7)) (Var (AstInt 7))
      b = BinaryOp NotEqual (Var (AstInt 3)) (Var (AstInt 4))
      operation = ListOfAst [BinaryOp Or, a, b]
      result = Var (AstBool "#t") -- 7 == 7 OR 3 != 4

testComplex4 :: Test
testComplex4 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #4" (Just expected) (result)
    where
      a = BinaryOp Modulo (Var (AstInt 20)) (Var (AstInt 6))
      b = BinaryOp Sub (Var (AstInt 8)) (Var (AstInt 4))
      operation = ListOfAst [BinaryOp LessThanOrEqual, a, b]
      result = Var (AstBool "#t") -- 20 % 6 >= 8

testComplex5 :: Test
testComplex5 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #5" (Just expected) (result)
    where
      a = BinaryOp Add (Var (AstInt 2)) (Var (AstInt 3))
      b = BinaryOp Multiply (Var (AstInt 4)) (Var (AstInt 5))
      c = BinaryOp Divide b (Var (AstInt 2))
      operation = ListOfAst [BinaryOp Sub, a, c]
      result = Var (AstInt (-5)) -- (2 + 3) - (4 * 5 / 2) = 5 - 10 = -5

testComplex6 :: Test
testComplex6 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #6" (Just expected) (result)
    where
      a = BinaryOp Add (Var (AstInt 10)) (Var (AstInt 20))
      b = BinaryOp Sub (Var (AstInt 5)) (Var (AstInt 3))
      operation = ListOfAst [BinaryOp Multiply, a, b]
      result = Var (AstInt 60) -- (10 + 20) * (5 - 3) = 30 * 2 = 60

testComplex7 :: Test
testComplex7 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #7" (Just expected) (result)
    where
      a = BinaryOp GreaterThan (Var (AstInt 7)) (Var (AstInt 3))
      b = BinaryOp LessThan (Var (AstInt 10)) (Var (AstInt 15))
      operation = ListOfAst [BinaryOp Or, a, b]
      result = Var (AstBool "#t") -- 7 > 3 OR 10 < 15

testComplex8 :: Test
testComplex8 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #8" (Just expected) (result)
    where
      a = BinaryOp Equal (Var (AstInt 9)) (Var (AstInt 9))
      b = BinaryOp NotEqual (Var (AstInt 10)) (Var (AstInt 10))
    operation = ListOfAst [BinaryOp And, a, b]
      result = Var (AstBool "#f") -- 9 == 9 AND 10 != 10

testComplex9 :: Test
testComplex9 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #9" (Just expected) (result)
    where
      a = BinaryOp Multiply (Var (AstInt 3)) (Var (AstInt 3))
      b = BinaryOp Modulo (Var (AstInt 17)) (Var (AstInt 5))
      operation = ListOfAst [BinaryOp GreaterThanOrEqual, a, b]
      result = Var (AstBool "#t") -- 9 >= 3 x 3

testComplex10 :: Test
testComplex10 = TestCase $ do
    let (result, _) = evaluation operation createEnv
    assertEqual "Complex Test #10" (Just expected) (result)
    where
      a = BinaryOp Divide (Var (AstInt 100)) (Var (AstInt 5))
      b = BinaryOp Multiply (Var (AstInt 3)) (Var (AstInt 2))
      operation = ListOfAst [BinaryOp Sub, a, b]
      result = Var (AstInt 14) -- (100 / 5) - (3 * 2) = 20 - 6 = 14
