module AST.Tree.ConvertToAst (
    convertSExprToAst,
) where

import AST.Constants

convertSExprToAst :: SExpr -> Ast
convertSExprToAst (SInt n) = Var (AstInt n)
convertSExprToAst (SSymb s) = Var (AstSymb s)
convertSExprToAst (SList [SSymb "define", SSymb name, valueExpr]) =
    Define name (convertSExprToAst valueExpr)
convertSExprToAst (SList [SSymb operator, operand1, operand2]) =
    BinaryOp (stringToBinaryOp operator) (convertSExprToAst operand1) (convertSExprToAst operand2)
convertSExprToAst _ = error "Invalid SExpr"

stringToBinaryOp :: String -> AstBinaryOp
stringToBinaryOp "+" = Add
stringToBinaryOp "*" = Mul
stringToBinaryOp "-" = Sub
stringToBinaryOp "/" = Div
stringToBinaryOp "%" = Mod
stringToBinaryOp "=" = Eq
stringToBinaryOp "/=" = NotEq
stringToBinaryOp "<" = Less
stringToBinaryOp "<=" = LessOrEq
stringToBinaryOp ">" = Greater
stringToBinaryOp ">=" = GreaterOrEq
stringToBinaryOp "&&" = And
stringToBinaryOp "||" = Or
stringToBinaryOp _ = error "Invalid binary operator"
