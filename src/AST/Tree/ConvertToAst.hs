module AST.Tree.ConvertToAst (
    convertSExprToAst,
) where

import AST.Constants

convertSExprToAst :: SExpr -> Ast
convertSExprToAst (SInt n) = Var (AstInt n)
convertSExprToAst (SSymb s) = Var (AstSymb s)
convertSExprToAst (SList [SSymb "define", SSymb name, valueExpr]) =
    Define name (convertSExprToAst valueExpr)
convertSExprToAst _ = error "Invalid SExpr"

