module AST.Tree.ConvertToAst (
    convertSExprToAst,
) where

import AST.Constants

convertSExprToAst :: SExpr -> Ast
convertSExprToAst (SInt n) = Var (AstInt n)
convertSExprToAst (SSymb s) = Var (AstSymb s)
convertSExprToAst (SList [SSymb "define", SSymb name, valueExpr]) =
    Define name (convertSExprToAst valueExpr)
convertSExprToAst (SList [SSymb "cond", condExpr, thenExpr, elseExpr]) =
    Cond (convertSExprToAst condExpr) (convertSExprToAst thenExpr) (convertSExprToAst elseExpr)
convertSExprToAst (SList [SSymb operator, operand1, operand2]) =
    BinaryOp (stringToBinaryOp operator) (convertSExprToAst operand1) (convertSExprToAst operand2)
convertSExprToAst _ = error "Invalid SExpr"

stringToBinaryOp :: String -> AstBinaryOp
stringToBinaryOp "+" = Add
stringToBinaryOp "*" = Multiply
stringToBinaryOp "-" = Sub
stringToBinaryOp "/" = Divide
stringToBinaryOp "%" = Modulo
stringToBinaryOp "=" = Equal
stringToBinaryOp "/=" = NotEqual
stringToBinaryOp "<" = LessThan
stringToBinaryOp "<=" = LessThanOrEqual
stringToBinaryOp ">" = GreaterThan
stringToBinaryOp ">=" = GreaterThanOrEqual
stringToBinaryOp "&&" = And
stringToBinaryOp "||" = Or
stringToBinaryOp _ = error "Invalid binary operator"
