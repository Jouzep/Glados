-- Evaluation.hs
module Evaluation.Evaluation (evaluation) where

import AST.Constants

evaluation :: Ast -> Maybe Ast
evaluation (Var n) = Just (Var (n))
evaluation (BinaryOp op args1 args2) = evalBinaryOp op args1 args2 -- Fix the pattern matching for BinaryOp
-- evaluation (BinaryOp op args args) = evalBinaryOp op args
-- evaluation (FunctionCall n args) = evalCallFunction n args
-- evalAST (Call "+" args) = evalBinaryOp "+" args
evaluation n = Just n


evalBinaryOp :: AstBinaryOp -> Ast -> Ast -> Maybe Ast
evalBinaryOp Add ast1 ast2 = do
    case (evaluation ast1, evaluation ast2) of
        (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (n1 + n2)))
        _ -> Nothing

evalBinaryOp Multiply ast1 ast2 = do
    case (evaluation ast1, evaluation ast2) of
        (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (n1 * n2)))
        _ -> Nothing

evalBinaryOp Sub ast1 ast2 = do
    case (evaluation ast1, evaluation ast2) of
        (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (n1 - n2)))
        _ -> Nothing

evalBinaryOp Divide ast1 ast2 = do
    case (evaluation ast1, evaluation ast2) of
        (Just (Var (AstInt n1)), Just (Var (AstInt n2))) ->
            if n2 /= 0 then Just (Var (AstInt (n1 `div` n2))) else Nothing
        _ -> Nothing

evalBinaryOp Modulo ast1 ast2 = do
    case (evaluation ast1, evaluation ast2) of
        (Just (Var (AstInt n1)), Just (Var (AstInt n2))) ->
            if n2 /= 0 then Just (Var (AstInt (n1 `mod` n2))) else Nothing
        _ -> Nothing

-- evalBinaryOp Equal ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 == n2 then 1 else 0)))
--         _ -> Nothing

-- evalBinaryOp NotEqual ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 /= n2 then 1 else 0)))
--         _ -> Nothing

-- evalBinaryOp LessThan ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 < n2 then 1 else 0)))
--         _ -> Nothing

-- evalBinaryOp LessThanOrEqual ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 <= n2 then 1 else 0)))
--         _ -> Nothing

-- evalBinaryOp GreaterThan ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 > n2 then 1 else 0)))
--         _ -> Nothing

-- evalBinaryOp GreaterThanOrEqual ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 >= n2 then 1 else 0)))
--         _ -> Nothing

-- evalBinaryOp And ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 /= 0 && n2 /= 0 then 1 else 0)))
--         _ -> Nothing

-- evalBinaryOp Or ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         (Just (Var (AstInt n1)), Just (Var (AstInt n2))) -> Just (Var (AstInt (if n1 /= 0 || n2 /= 0 then 1 else 0)))
--         _ -> Nothing

-- Add more cases for other operators as needed

evalBinaryOp _ _ _ = Nothing


-- test :: Ast -> Ast