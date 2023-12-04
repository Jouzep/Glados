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
sexprToAST (ListExpress [SymbolExpression "+", var, args]) = 
  case (sexprToAST args, sexprToAST var) of
    (Just astExpr, Just astExpre )-> Just (Call "+" [astExpre, astExpr])
    (Nothing, Nothing) -> Nothing
sexprToAST (ListExpress [SymbolExpression "*", var, args]) = 
  case (sexprToAST args, sexprToAST var) of
    (Just astExpr, Just astExpre )-> Just (Call "+" [astExpre, astExpr])
    (Nothing, Nothing) -> Nothing
sexprToAST _ = Nothing
_ = Nothing


evalAST :: Ast -> Maybe Ast
evalAST (IntLiteral n) = Just (IntLiteral n)
evalAST (Symbol _) = Nothing

evalAST (Define var expr) = do
    value <- evalAST expr
    Just (Define var value)

evalAST (Call "+" args) = do
    argValues <- traverse evalAST args
    case argValues of
        [IntLiteral a, IntLiteral b] -> Just (IntLiteral (a + b))
        _ -> Nothing

evalAST (Call "*" args) = do
    argValues <- traverse evalAST args
    case argValues of
        [IntLiteral a, IntLiteral b] -> Just (IntLiteral (a * b))
        _ -> Nothing

evalAST (Call "-" args) = do
    argValues <- traverse evalAST args
    case argValues of
        [IntLiteral a, IntLiteral b] -> Just (IntLiteral (a - b))
        _ -> Nothing

evalAST (Call func _) =
    Just $ Symbol $ "Unrecognized function: " ++ func


-- Just (Define "x" (IntLiteral 5))
-- *Lib> let a = ListExpress [SymbolExpression "define", SymbolExpression "x", IntExpression 5]
-- *Lib> let b  = ListExpress [SymbolExpression "+", Int]
-- Int            IntExpression  IntLiteral     Integer        Integral
-- *Lib> let b  = ListExpress [SymbolExpression "+", SymbolExpression "x", IntExpression 4]
-- *Lib> sexprToAST b
-- Just (Call "+" [Symbol "x",IntLiteral 4])
-- *Lib> let c  = ListExpress [SymbolExpression "+", SymbolExpression "x", IntExpression 4, IntExpression 5]
-- *Lib> sexprToAST c
-- Nothing
-- *Lib> let b = ListExpress [SymbolExpression "+", SymbolExpression "x", ListExpress [SymbolExpression "*", IntExpression 4, SymbolExpression "y"]]
-- *Lib> sexprToAST b
-- Just (Call "+" [Symbol "x",Call "+" [IntLiteral 4,Symbol "y"]])
-- *Lib> let c = ListExpress [SymbolExpression "Define", SymbolExpression "fourtyTwo", ListExpress [SymbolExpression "*", IntExpression 7, Int]]
-- Int            IntExpression  IntLiteral     Integer        Integral
-- *Lib> let c = ListExpress [SymbolExpression "Define", SymbolExpression "fourtyTwo", ListExpress [SymbolExpression "*", IntExpression 7, IntExpression 6]]
-- *Lib> sexprToAST c
-- Nothing
-- *Lib> let c = ListExpress [SymbolExpression "define", SymbolExpression "fourtyTwo", ListExpress [SymbolExpression "*", IntExpression 7, IntExpression 6]]
-- *Lib> sexprToAST c
-- Just (Define "fourtyTwo" (Call "+" [IntLiteral 7,IntLiteral 6]))

-- evalAST :: Ast -> Maybe Ast