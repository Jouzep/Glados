
module AstTest (astTests) where

import Test.HUnit
import Lib
import AST.Constants
import AST.Tree.ConvertToAst

astTests :: Test
astTests = TestList [arithmeticTests, specificExpressionTests]

arithmeticTests :: Test
arithmeticTests = TestList
  [ "Test Add" ~: convertSExprToAst (SList [SSymb "+", SInt 2, SInt 3]) ~?= BinaryOp Add (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Mul" ~: convertSExprToAst (SList [SSymb "*", SInt 2, SInt 3]) ~?= BinaryOp Multiply (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Sub" ~: convertSExprToAst (SList [SSymb "-", SInt 2, SInt 3]) ~?= BinaryOp Sub (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Div" ~: convertSExprToAst (SList [SSymb "/", SInt 2, SInt 3]) ~?= BinaryOp Divide (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Mod" ~: convertSExprToAst (SList [SSymb "%", SInt 2, SInt 3]) ~?= BinaryOp Modulo (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Eq" ~: convertSExprToAst (SList [SSymb "=", SInt 2, SInt 3]) ~?= BinaryOp Equal (Var (AstInt 2)) (Var (AstInt 3))
  , "Test NotEq" ~: convertSExprToAst (SList [SSymb "/=", SInt 2, SInt 3]) ~?= BinaryOp NotEqual (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Less" ~: convertSExprToAst (SList [SSymb "<", SInt 2, SInt 3]) ~?= BinaryOp LessThan (Var (AstInt 2)) (Var (AstInt 3))
  , "Test LessOrEq" ~: convertSExprToAst (SList [SSymb "<=", SInt 2, SInt 3]) ~?= BinaryOp LessThanOrEqual (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Greater" ~: convertSExprToAst (SList [SSymb ">", SInt 2, SInt 3]) ~?= BinaryOp GreaterThan (Var (AstInt 2)) (Var (AstInt 3))
  , "Test GreatorEq" ~: convertSExprToAst (SList [SSymb ">=", SInt 2, SInt 3]) ~?= BinaryOp GreaterThanOrEqual (Var (AstInt 2)) (Var (AstInt 3))
  , "Test And" ~: convertSExprToAst (SList [SSymb "&&", SInt 2, SInt 3]) ~?= BinaryOp And (Var (AstInt 2)) (Var (AstInt 3))
  , "Test Or" ~: convertSExprToAst (SList [SSymb "||", SInt 2, SInt 3]) ~?= BinaryOp Or (Var (AstInt 2)) (Var (AstInt 3))
  ]

specificExpressionTests :: Test
specificExpressionTests = TestList
  [ "Test Define" ~: convertSExprToAst (SList [SSymb "define", SSymb "x", SInt 42]) ~?= Define "x" (Var (AstInt 42))
  ]

