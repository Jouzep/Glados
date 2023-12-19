module EvaluationTests.EvaluationSimpleOperator (evalSimpleOperatorTests) where

import Test.HUnit
import Lib
import AST.Constants
import Evaluation.Evaluation (evaluation)
evalSimpleOperatorTests :: Test
evalSimpleOperatorTests = TestList [testEvalAdd1]


testEvalAdd1 :: Test
testEvalAdd1 = TestCase $ do
  let a = Var (AstInt 5)
      b = Var (AstInt 5)
      operation = BinaryOp Add a b
      result = Var (AstInt 10)
  assertEqual "Eval Test #1" (Just result) (evaluation operation)
  where
    value1 :: Ast
    value1 = Var (AstInt 5)
    value2 :: Ast
    value2 = Var (AstInt 5)
    operation :: Ast
    operation = BinaryOp Add value1 value2
    result :: Ast
    result = Var (AstInt 10)

    expected :: Maybe Ast
    expected = evaluation operation
    actual :: Maybe Ast
    actual = Just result

-- arithmeticTests :: Test
-- arithmeticTests = TestList
--   [ "Test Add" ~: evaluation (BinaryOp Add (Var (AstInt 5)) (Var (AstInt 5))) ~?= Just (Var (AstInt 10))
--   ]