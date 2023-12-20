module EvaluationTests.ComplexTests.EvaluationComplexTests (evalComplexTests) where

import Test.HUnit
import AST.Constants
import Evaluation.Evaluation (evaluation)

evalComplexTests :: Test
evalComplexTests = TestList
    [testComplex1, testComplex2, testComplex3, testComplex4, testComplex5, testComplex6, testComplex7, testComplex8, testComplex9, testComplex10]

testComplex1 :: Test
testComplex1 = TestCase $ do
    assertEqual "Complex Test #1" (Just result1) (evaluation operation1)
    where
      a1 = BinaryOp Add (Var (AstInt 10)) (Var (AstInt 5))
      b1 = BinaryOp Multiply (Var (AstInt 2)) (Var (AstInt 3))
      operation1 = BinaryOp Divide a1 b1
      result1 = Var (AstInt 2) -- (10 + 5) / (2 * 3) = 15 / 6 = 2

testComplex2 :: Test
testComplex2 = TestCase $ do
    assertEqual "Complex Test #2" (Just result2) (evaluation operation2)
    where
      a2 = BinaryOp GreaterThan (Var (AstInt 15)) (Var (AstInt 10))
      b2 = BinaryOp LessThan (Var (AstInt 5)) (Var (AstInt 8))
      operation2 = BinaryOp And a2 b2
      result2 = Var (AstBool "#t") -- 15 > 10 AND 5 < 8

testComplex3 :: Test
testComplex3 = TestCase $ do
    assertEqual "Complex Test #3" (Just result3) (evaluation operation3)
    where
      a3 = BinaryOp Equal (Var (AstInt 7)) (Var (AstInt 7))
      b3 = BinaryOp NotEqual (Var (AstInt 3)) (Var (AstInt 4))
      operation3 = BinaryOp Or a3 b3
      result3 = Var (AstBool "#t") -- 7 == 7 OR 3 != 4

testComplex4 :: Test
testComplex4 = TestCase $ do
    assertEqual "Complex Test #4" (Just result4) (evaluation operation4)
    where
      a4 = BinaryOp Modulo (Var (AstInt 20)) (Var (AstInt 6))
      b4 = BinaryOp Sub (Var (AstInt 8)) (Var (AstInt 4))
      operation4 = BinaryOp LessThanOrEqual a4 b4
      result4 = Var (AstBool "#t") -- 20 % 6 >= 8

testComplex5 :: Test
testComplex5 = TestCase $ do
    assertEqual "Complex Test #5" (Just result5) (evaluation operation5)
    where
      a5 = BinaryOp Add (Var (AstInt 2)) (Var (AstInt 3))
      b5 = BinaryOp Multiply (Var (AstInt 4)) (Var (AstInt 5))
      c5 = BinaryOp Divide b5 (Var (AstInt 2))
      operation5 = BinaryOp Sub a5 c5
      result5 = Var (AstInt (-5)) -- (2 + 3) - (4 * 5 / 2) = 5 - 10 = -5

testComplex6 :: Test
testComplex6 = TestCase $ do
    assertEqual "Complex Test #6" (Just result6) (evaluation operation6)
    where
      a6 = BinaryOp Add (Var (AstInt 10)) (Var (AstInt 20))
      b6 = BinaryOp Sub (Var (AstInt 5)) (Var (AstInt 3))
      operation6 = BinaryOp Multiply a6 b6
      result6 = Var (AstInt 60) -- (10 + 20) * (5 - 3) = 30 * 2 = 60

testComplex7 :: Test
testComplex7 = TestCase $ do
    assertEqual "Complex Test #7" (Just result7) (evaluation operation7)
    where
      a7 = BinaryOp GreaterThan (Var (AstInt 7)) (Var (AstInt 3))
      b7 = BinaryOp LessThan (Var (AstInt 10)) (Var (AstInt 15))
      operation7 = BinaryOp Or a7 b7
      result7 = Var (AstBool "#t") -- 7 > 3 OR 10 < 15

testComplex8 :: Test
testComplex8 = TestCase $ do
    assertEqual "Complex Test #8" (Just result8) (evaluation operation8)
    where
      a8 = BinaryOp Equal (Var (AstInt 9)) (Var (AstInt 9))
      b8 = BinaryOp NotEqual (Var (AstInt 10)) (Var (AstInt 10))
      operation8 = BinaryOp And a8 b8
      result8 = Var (AstBool "#f") -- 9 == 9 AND 10 != 10

testComplex9 :: Test
testComplex9 = TestCase $ do
    assertEqual "Complex Test #9" (Just result9) (evaluation operation9)
    where
      a9 = BinaryOp Multiply (Var (AstInt 3)) (Var (AstInt 3))
      b9 = BinaryOp Modulo (Var (AstInt 17)) (Var (AstInt 5))
      operation9 = BinaryOp GreaterThanOrEqual a9 b9
      result9 = Var (AstBool "#t") -- 9 >= 3 x 3

testComplex10 :: Test
testComplex10 = TestCase $ do
    assertEqual "Complex Test #10" (Just result10) (evaluation operation10)
    where
      a10 = BinaryOp Divide (Var (AstInt 100)) (Var (AstInt 5))
      b10 = BinaryOp Multiply (Var (AstInt 3)) (Var (AstInt 2))
      operation10 = BinaryOp Sub a10 b10
      result10 = Var (AstInt 14) -- (100 / 5) - (3 * 2) = 20 - 6 = 14
