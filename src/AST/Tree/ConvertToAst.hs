module AST.Tree.ConvertToAst (
    convertSExprToAst,
) where

import AST.Constants

convertSExprToAst :: SExpr -> Ast
convertSExprToAst (SInt n) = Var (AstInt n)
convertSExprToAst (SSymb s) = Var (AstSymb s)
convertSExprToAst _ = error "Invalid SExpr"

