module Lib
    ( someFunc
    ) where

data SExpr 
    = IntExpression Int
    | SymbolExpression String
    | ListExpress [SExpr]
    deriving Show


-- data Ast = Define ? ?
-- | ? Int
-- ...
-- deriving Show

data Ast
  = Define  String Ast
  | IntLiteral Int
  | Symbol { symbolValue :: String }
  | Boolean { boolValue :: Bool }
  deriving (Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- printTree :: SExpr -> Maybe String

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

-- printTree :: SExpr -> Maybe String
-- printTree (IntExpression n) = Just $ "an Integer " ++ show n
-- printTree (SymbolExpression s) = Just $ "a Symbol '" ++ s ++ "'"
-- printTree (ListExpress []) = Nothing
-- printTree (ListExpress [expr]) = printTree expr
-- printTree (ListExpress (expr : rest)) = Just $ "a List with " ++ printTree expr ++ ", " ++ printTree (ListExpress rest)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (IntExpression n) = Just (IntLiteral n)
sexprToAST (SymbolExpression n) = Just (Symbol n)
sexprToAST (SymbolExpression "Define") = Just (Define "Define" (sexprToAST n))
sexprToAST (ListExpress [n]) = sexprToAST n
_ = Nothing
