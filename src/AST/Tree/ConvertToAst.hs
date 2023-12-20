module AST.Tree.ConvertToAst (
    convertSExprToAst,
    stringToBinaryOp
) where

import AST.Constants

convertSExprToAst ::SExpr -> Ast
convertSExprToAst (SInt n) = Var (AstInt n)
convertSExprToAst (SSymb "define") = Define
convertSExprToAst (SSymb "if") = If
convertSExprToAst (SSymb "lambda") = Lambda
convertSExprToAst (SSymb "functionCall") = FunctionCall
convertSExprToAst (SSymb "#t") = Var (AstBool "#t")
convertSExprToAst (SSymb "#f") = Var (AstBool "#f")
convertSExprToAst (SSymb s) =  case stringToBinaryOp s of
    Nothing -> Var (AstSymb s)
    Just x -> BinaryOp x
convertSExprToAst (SList exprs) = ListOfAst (map convertSExprToAst exprs)

stringToBinaryOp :: String -> Maybe AstBinaryOp
stringToBinaryOp "+" = Just Add
stringToBinaryOp "*" = Just Multiply
stringToBinaryOp "-" = Just Sub
stringToBinaryOp "/" = Just Divide
stringToBinaryOp "%" = Just Modulo
stringToBinaryOp "=" = Just Equal
stringToBinaryOp "/=" = Just NotEqual
stringToBinaryOp "<" = Just LessThan
stringToBinaryOp "<=" = Just LessThanOrEqual
stringToBinaryOp ">" = Just GreaterThan
stringToBinaryOp ">=" = Just GreaterThanOrEqual
stringToBinaryOp "&&" = Just And
stringToBinaryOp "||" = Just Or
stringToBinaryOp _ = Nothing
