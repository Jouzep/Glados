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

type Parser a = String -> Maybe (a, String)

parseChar :: Char -> Parser Char
parseChar expectedChar (c:rest) | c == expectedChar = Just (c, rest)
parseChar _ _ = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar (a:aRest) (b:bRest) | a == b = Just(a, bRest)
parseAnyChar (x:xs) str = parseAnyChar xs str
parseAnyChar _ _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 input =
  case parser1 input of
    Just result -> Just result
    Nothing     -> parser2 input

parseAnd :: Parser a -> Parser b -> Parser (a , b )
parseAnd parser1 parser2 input =
  case parser1 input of
    Just (result1, rest) ->
      case parser2 rest of
        Just (result2, rest2) -> Just ((result1, result2), rest2)
        Nothing               -> Nothing
    Nothing -> Nothing

parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith combine parser1 parser2 input =
  case parseAnd parser1 parser2 input of
    Just ((result1, result2), rest) -> Just (combine result1 result2, rest)
    Nothing                         -> Nothing

parseMany :: Parser a -> Parser [ a ]
parseMany parser input =
  case parser input of
    Just (result, rest) ->
      case parseMany parser rest of
        Just (results, rest2) -> Just (result : results, rest2)
        Nothing               -> Just ([result], rest)
    Nothing -> Just ([], input)