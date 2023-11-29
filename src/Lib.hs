module Lib
    ( someFunc
    ) where

data SExpr 
    = IntExpression Int
    | SymbolExpression String
    | ListExpress [SExpr]
    deriving Show


data Ast
  = Define  String Ast
  | IntLiteral Int
  | Symbol  String
  -- | Add Ast Ast
  -- | Multiply Ast Ast
  | Boolean  Bool
  | Call String [Ast]

  deriving (Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getSymbol :: SExpr -> Maybe String
getSymbol (SymbolExpression symbol) = Just symbol
getSymbol _             = Nothing

printTree :: SExpr -> Maybe String
printTree (IntExpression n) = Just $ "an Integer " ++ show n
printTree (SymbolExpression s) = Just $ "a Symbol '" ++ s ++ "'"
printTree (ListExpress []) = Nothing
printTree (ListExpress [expr]) = printTree expr
printTree (ListExpress (expr : rest)) =
  case (printTree expr, printTree (ListExpress rest)) of
    (Just str1, Just str2) -> Just $ "a List with " ++ str1 ++ ", " ++ str2
    _                      -> Nothing

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (IntExpression n) = Just (IntLiteral n)
sexprToAST (SymbolExpression n) = Just (Symbol n)
sexprToAST (ListExpress [SymbolExpression "define", SymbolExpression var, expr]) =
  case sexprToAST expr of
    Just astExpr -> Just (Define var astExpr)
    Nothing      -> Nothing
-- sexprToAST (ListExpress [SymbolExpression "+", SymbolExpression var, expr]) =
--   case sexprToAST expr of
--     Just astExpr -> Just (Call var [Add astExpr])
--     Nothing      -> Nothing
sexprToAST (ListExpress [SymbolExpression "+", SymbolExpression var, args]) = 
  case sexprToAST args of
    Just astExpr -> Just (Call "+" [Symbol var, astExpr])
    Nothing -> Nothing
sexprToAST _ = Nothing
_ = Nothing