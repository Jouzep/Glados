module AstTest (astTests) where

import Test.HUnit
import AST.Constants
import AST.Tree.ConvertToAst

astTests :: Test
astTests = TestList [simpleArithmeticTests, specificExpressionTests, pdfExpressionTests, moreExpressionTests]

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

pdfExpressionTests :: Test
pdfExpressionTests = TestList
  [ "Test If True" ~: convertSExprToAst (SList [SSymb "if", SSymb "#t", SInt 1, SInt 2]) ~?= ListOfAst [If, Var (AstBool "#t"), Var (AstInt 1), Var (AstInt 2)]
  , "Test If False" ~: convertSExprToAst (SList [SSymb "if", SSymb "#f", SInt 1, SInt 2]) ~?= ListOfAst [If, Var (AstBool "#f"), Var (AstInt 1), Var (AstInt 2)]
  , "Test Define and If" ~: convertSExprToAst (SList [SSymb "define", SSymb "foo", SInt 42, SSymb "if", SList [SSymb "<", SSymb "foo", SInt 10], SList [SSymb "*", SSymb "foo", SInt 3], SList [SSymb "/", SSymb "foo", SInt 2]]) ~?= ListOfAst [Define, Var (AstSymb "foo"), Var (AstInt 42), If, ListOfAst [BinaryOp LessThan, Var (AstSymb "foo"), Var (AstInt 10)], ListOfAst [BinaryOp Multiply, Var (AstSymb "foo"), Var (AstInt 3)], ListOfAst [BinaryOp Divide, Var (AstSymb "foo"), Var (AstInt 2)]]
  , "Test Complex Expression" ~: convertSExprToAst (SList [SSymb "+", SList [SSymb "*", SInt 2, SInt 3], SList [SSymb "/", SInt 10, SInt 2]]) ~?= ListOfAst [BinaryOp Add, ListOfAst [BinaryOp Multiply, Var (AstInt 2), Var (AstInt 3)], ListOfAst [BinaryOp Divide, Var (AstInt 10), Var (AstInt 2)]]
  , "Test Equal" ~: convertSExprToAst (SList [SSymb "=", SList [SSymb "*", SInt 2, SInt 5], SList [SSymb "-", SInt 11, SInt 1]]) ~?= ListOfAst [BinaryOp Equal, ListOfAst [BinaryOp Multiply, Var (AstInt 2), Var (AstInt 5)], ListOfAst [BinaryOp Sub, Var (AstInt 11), Var (AstInt 1)]]
  , "Test Modulo" ~: convertSExprToAst (SList [SSymb "<", SInt 1, SList [SSymb "%", SInt 10, SInt 3]]) ~?= ListOfAst [BinaryOp LessThan, Var (AstInt 1), ListOfAst [BinaryOp Modulo, Var (AstInt 10), Var (AstInt 3)]]
  , "Test Greater Than" ~: convertSExprToAst (SList [SSymb "define", SList [SSymb ">", SSymb "a", SSymb "b"], SList [SSymb "if", SList [SSymb "=", SSymb "a", SSymb "b"], SSymb "#f", SList [SSymb "if", SList [SSymb "<", SSymb "a", SSymb "b"], SSymb "#f", SSymb "#t"]]]) ~?= ListOfAst [Define, ListOfAst [BinaryOp GreaterThan, Var (AstSymb "a"), Var (AstSymb "b")], ListOfAst [If, ListOfAst [BinaryOp Equal, Var (AstSymb "a"), Var (AstSymb "b")], Var (AstBool "#f"), ListOfAst [If, ListOfAst [BinaryOp LessThan, Var (AstSymb "a"), Var (AstSymb "b")], Var (AstBool "#f"), Var (AstBool "#t")]]]
  , "Test Factorial Function" ~: convertSExprToAst (SList [SSymb "define", SList [SSymb "fact", SSymb "x"], SList [SSymb "if", SList [SSymb "=", SSymb "x", SInt 1], SInt 1, SList [SSymb "*", SSymb "x", SList [SSymb "fact", SList [SSymb "-", SSymb "x", SInt 1]]]], SList [SSymb "fact", SInt 5]]) ~?= ListOfAst [Define, ListOfAst [Var (AstSymb "fact"), Var (AstSymb "x")], ListOfAst [If, ListOfAst [BinaryOp Equal, Var (AstSymb "x"), Var (AstInt 1)], Var (AstInt 1), ListOfAst [BinaryOp Multiply, Var (AstSymb "x"), ListOfAst [Var (AstSymb "fact"), ListOfAst [BinaryOp Sub, Var (AstSymb "x"), Var (AstInt 1)]]]], ListOfAst [Var (AstSymb "fact"), Var (AstInt 5)]]
  ]

moreExpressionTests :: Test
moreExpressionTests = TestList
  [ "Test LessThanOrEqual" ~: convertSExprToAst (SList [SSymb "<=", SInt 2, SInt 3]) ~?= ListOfAst [BinaryOp LessThanOrEqual, Var (AstInt 2), Var (AstInt 3)]
  , "Test GreaterThanOrEqual" ~: convertSExprToAst (SList [SSymb ">=", SInt 5, SInt 3]) ~?= ListOfAst [BinaryOp GreaterThanOrEqual, Var (AstInt 5), Var (AstInt 3)]
  , "Test Complex If" ~: convertSExprToAst (SList [SSymb "if", SList [SSymb ">", SInt 5, SInt 3], SInt 10, SInt 20]) ~?= ListOfAst [If, ListOfAst [BinaryOp GreaterThan, Var (AstInt 5), Var (AstInt 3)], Var (AstInt 10), Var (AstInt 20)]
  , "Test Nested If" ~: convertSExprToAst (SList [SSymb "if", SList [SSymb "=", SInt 1, SInt 1], SList [SSymb "if", SList [SSymb "<", SInt 2, SInt 3], SInt 100, SInt 200], SInt 42]) ~?= ListOfAst [If, ListOfAst [BinaryOp Equal, Var (AstInt 1), Var (AstInt 1)], ListOfAst [If, ListOfAst [BinaryOp LessThan, Var (AstInt 2), Var (AstInt 3)], Var (AstInt 100), Var (AstInt 200)], Var (AstInt 42)]
  , "Test Multiple If Conditions" ~: convertSExprToAst (SList [SSymb "if", SList [SSymb "=", SInt 1, SInt 1], SInt 10, SList [SSymb "if", SList [SSymb ">", SInt 2, SInt 3], SInt 100, SList [SSymb "if", SList [SSymb "<", SInt 5, SInt 10], SInt 500, SInt 1000]]]) ~?= ListOfAst [If, ListOfAst [BinaryOp Equal, Var (AstInt 1), Var (AstInt 1)], Var (AstInt 10), ListOfAst [If, ListOfAst [BinaryOp GreaterThan, Var (AstInt 2), Var (AstInt 3)], Var (AstInt 100), ListOfAst [If, ListOfAst [BinaryOp LessThan, Var (AstInt 5), Var (AstInt 10)], Var (AstInt 500), Var (AstInt 1000)]]]
  ]
