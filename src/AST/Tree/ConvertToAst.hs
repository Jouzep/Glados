module AST.Tree.ConvertToAst (
    convertChiasseToAst,
    convertAllChiasseToAst
) where

import AST.Constants
import Lib

findValue :: [String] -> Maybe Ast
findValue (v1:"+":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "add" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"*":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "multiply" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"-":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "sub" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"/":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "divide" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"%":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "modulo" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"=":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "equal" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"/=":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "notEqual" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"<":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "lessThan" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"<=":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "lessThanOrEqual" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:">":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "greaterThan" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:">=":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "greaterThanOrEqual" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"&&":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "and" [(Constant v1), ast])
    Nothing -> Nothing
findValue (v1:"||":v2) = case findValue v2 of
    Just ast -> Just (FunctionCall "or" [(Constant v1), ast])
    Nothing -> Nothing
findValue (x:_) = Just (Constant x)

findLine :: [String] -> Maybe Ast
findLine ("let":name:_:xs) = case sequence (map findValue [xs]) of
    Just asts -> Just (Define name asts)
    Nothing -> Nothing
findLine ("return":value) = case sequence (map findValue [value]) of
    Just asts -> Just (Return asts)
    Nothing -> Nothing
findLine _ = Nothing

convertChiasseToAst :: Chiasse -> Maybe Ast
convertChiasseToAst (CFunction (["function", name], args, body)) = case  sequence (map convertChiasseToAst body) of
    Just asts -> Just (FunctionDefinition name args asts)
    Nothing -> Nothing
convertChiasseToAst (CCall (name, args)) = Just (FunctionCall name (map (\x -> Constant x) args))
convertChiasseToAst (CLine exprs) = case findLine exprs of
    Just ast -> Just ast
    Nothing -> Nothing
convertChiasseToAst (CIf (condition, body, bodyElse)) = case sequence (map convertChiasseToAst body) of
    Just asts -> case sequence (map convertChiasseToAst bodyElse) of
        Just astsElse -> Just (Conditional condition asts astsElse)
        Nothing -> Nothing
    Nothing -> Nothing

convertAllChiasseToAst :: [Chiasse] -> Maybe [Ast]
convertAllChiasseToAst content = case sequence (map convertChiasseToAst content) of
    Just asts -> Just asts
    Nothing -> Nothing

-- stringToBinaryOp :: String -> Maybe AstBinaryOp
-- stringToBinaryOp "+" = Just Add
-- stringToBinaryOp "*" = Just Multiply
-- stringToBinaryOp "-" = Just Sub
-- stringToBinaryOp "/" = Just Divide
-- stringToBinaryOp "%" = Just Modulo
-- stringToBinaryOp "=" = Just Equal
-- stringToBinaryOp "/=" = Just NotEqual
-- stringToBinaryOp "<" = Just LessThan
-- stringToBinaryOp "<=" = Just LessThanOrEqual
-- stringToBinaryOp ">" = Just GreaterThan
-- stringToBinaryOp ">=" = Just GreaterThanOrEqual
-- stringToBinaryOp "&&" = Just And
-- stringToBinaryOp "||" = Just Or
-- stringToBinaryOp _ = Nothing
