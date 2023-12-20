module AstTest (astTests) where

import Test.HUnit
import AST.Constants
import AST.Tree.ConvertToAst

astTests :: Test
astTests = TestList [simpleArithmeticTests, specificExpressionTests]

simpleArithmeticTests :: Test
simpleArithmeticTests = TestList
  [ "Test Add" ~: convertSExprToAst (SList [SSymb "+", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Add, Var (AstInt 2), Var (AstInt 3)]
  , "Test Mul" ~: convertSExprToAst (SList [SSymb "*", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Multiply, Var (AstInt 2), Var (AstInt 3)]
  , "Test Sub" ~: convertSExprToAst (SList [SSymb "-", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Sub, Var (AstInt 2), Var (AstInt 3)]
  , "Test Div" ~: convertSExprToAst (SList [SSymb "/", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Divide, Var (AstInt 2), Var (AstInt 3)]
  , "Test Mod" ~: convertSExprToAst (SList [SSymb "%", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Modulo, Var (AstInt 2), Var (AstInt 3)]
  , "Test Eq" ~: convertSExprToAst (SList [SSymb "=", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Equal, Var (AstInt 2), Var (AstInt 3)]
  , "Test NotEq" ~: convertSExprToAst (SList [SSymb "/=", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp NotEqual, Var (AstInt 2), Var (AstInt 3)]
  , "Test Less" ~: convertSExprToAst (SList [SSymb "<", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp LessThan, Var (AstInt 2), Var (AstInt 3)]
  , "Test LessOrEq" ~: convertSExprToAst (SList [SSymb "<=", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp LessThanOrEqual, Var (AstInt 2), Var (AstInt 3)]
  , "Test Greater" ~: convertSExprToAst (SList [SSymb ">", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp GreaterThan, Var (AstInt 2), Var (AstInt 3)]
  , "Test GreatorEq" ~: convertSExprToAst (SList [SSymb ">=", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp GreaterThanOrEqual, Var (AstInt 2), Var (AstInt 3)]
  , "Test And" ~: convertSExprToAst (SList [SSymb "&&", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp And, Var (AstInt 2), Var (AstInt 3)]
  , "Test Or" ~: convertSExprToAst (SList [SSymb "||", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Or, Var (AstInt 2), Var (AstInt 3)]
  ]

specificExpressionTests :: Test
specificExpressionTests = TestList
  [ "Test Define" ~: convertSExprToAst (SList [SSymb "define", SSymb "x", SInt 42]) ~?= ListOfAst [Define, Var (AstSymb "x"), Var (AstInt 42)]
  , "Test If" ~: convertSExprToAst (SList [SSymb "if", SInt 1, SInt 2, SInt 3]) ~?= ListOfAst [If, Var (AstInt 1), Var (AstInt 2), Var (AstInt 3)]
  , "Test Lambda" ~: convertSExprToAst (SList [SSymb "lambda", SList [SSymb "x"], SList [SSymb "+", SSymb "x", SInt 1]]) ~?= ListOfAst [Lambda,ListOfAst [Var (AstSymb "x")],ListOfAst [BinaryOp Add,Var (AstSymb "x"),Var (AstInt 1)]]
  , "Test FunctionCall" ~: convertSExprToAst (SList [SSymb "functionCall", SSymb "add", SInt 2, SInt 3]) ~?= ListOfAst [FunctionCall,Var (AstSymb "add"),Var (AstInt 2),Var (AstInt 3)]
  , "Test Nested Expressions" ~: convertSExprToAst (SList [SSymb "+", SInt 1, SList [SSymb "*", SInt 2, SInt 3]]) ~?= ListOfAst [BinaryOp Add,Var (AstInt 1),ListOfAst [BinaryOp Multiply,Var (AstInt 2),Var (AstInt 3)]]
  , "Test BinaryOp" ~: convertSExprToAst (SList [SSymb "+", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp Add,Var (AstInt 2),Var (AstInt 3)]
  , "Test Multiple Arguments FunctionCall" ~: convertSExprToAst (SList [SSymb "functionCall", SSymb "multiply", SInt 2, SInt 3, SInt 4]) ~?= ListOfAst [FunctionCall,Var (AstSymb "multiply"),Var (AstInt 2),Var (AstInt 3),Var (AstInt 4)]
  ]
