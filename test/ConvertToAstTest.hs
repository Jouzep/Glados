module ConvertToAstTest (convertToAstTests, stringToBinaryOpTests) where

import Test.HUnit
import AST.Constants
import AST.Tree.ConvertToAst

convertToAstTests :: Test
convertToAstTests = TestList [testInt, testDefine, testIf, testLambda, testFunctionCall, testBoolTrue, testBoolFalse, testSymbol, testBinaryOp, testList]

testInt :: Test
testInt = TestCase $ assertEqual "Integer conversion" (Var (AstInt 42)) (convertSExprToAst (SInt 42))

testDefine :: Test
testDefine = TestCase $ assertEqual "Define conversion" Define (convertSExprToAst (SSymb "define"))

testIf :: Test
testIf = TestCase $ assertEqual "If conversion" If (convertSExprToAst (SSymb "if"))

testLambda :: Test
testLambda = TestCase $ assertEqual "Lambda conversion" Lambda (convertSExprToAst (SSymb "lambda"))

testFunctionCall :: Test
testFunctionCall = TestCase $ assertEqual "FunctionCall conversion" FunctionCall (convertSExprToAst (SSymb "functionCall"))

testBoolTrue :: Test
testBoolTrue = TestCase $ assertEqual "Boolean True conversion" (Var (AstBool "#t")) (convertSExprToAst (SSymb "#t"))

testBoolFalse :: Test
testBoolFalse = TestCase $ assertEqual "Boolean False conversion" (Var (AstBool "#f")) (convertSExprToAst (SSymb "#f"))

testSymbol :: Test
testSymbol = TestCase $ assertEqual "Symbol conversion" (Var (AstSymb "x")) (convertSExprToAst (SSymb "x"))

testBinaryOp :: Test
testBinaryOp = TestCase $ assertEqual "Binary operator conversion" (BinaryOp Add) (convertSExprToAst (SSymb "+"))

testList :: Test
testList = TestCase $ assertEqual "List conversion"
  (ListOfAst [Var (AstInt 1), Var (AstInt 2), BinaryOp Add])
  (convertSExprToAst (SList [SInt 1, SInt 2, SSymb "+"]))

stringToBinaryOpTests :: Test
stringToBinaryOpTests = TestList
  [ TestCase $ assertEqual "Add conversion" (Just Add) (stringToBinaryOp "+")
  , TestCase $ assertEqual "Multiply conversion" (Just Multiply) (stringToBinaryOp "*")
  , TestCase $ assertEqual "Sub conversion" (Just Sub) (stringToBinaryOp "-")
  , TestCase $ assertEqual "Divide conversion" (Just Divide) (stringToBinaryOp "/")
  , TestCase $ assertEqual "Modulo conversion" (Just Modulo) (stringToBinaryOp "%")
  , TestCase $ assertEqual "Equal conversion" (Just Equal) (stringToBinaryOp "=")
  , TestCase $ assertEqual "NotEqual conversion" (Just NotEqual) (stringToBinaryOp "/=")
  , TestCase $ assertEqual "LessThan conversion" (Just LessThan) (stringToBinaryOp "<")
  , TestCase $ assertEqual "LessThanOrEqual conversion" (Just LessThanOrEqual) (stringToBinaryOp "<=")
  , TestCase $ assertEqual "GreaterThan conversion" (Just GreaterThan) (stringToBinaryOp ">")
  , TestCase $ assertEqual "GreaterThanOrEqual conversion" (Just GreaterThanOrEqual) (stringToBinaryOp ">=")
  , TestCase $ assertEqual "And conversion" (Just And) (stringToBinaryOp "&&")
  , TestCase $ assertEqual "Or conversion" (Just Or) (stringToBinaryOp "||")
  ]
