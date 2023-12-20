-- Evaluation.hs
module Evaluation.Evaluation (evaluation) where

import AST.Constants
import AST.Env

evaluation :: Ast -> Env -> (Maybe Ast, Env)
evaluation (Var (AstSymb n)) myEnv = ((evalVarCall n myEnv), myEnv)
evaluation (Var n) myEnv = (Just (Var n), myEnv)
evaluation (ListOfAst (BinaryOp op:args1:args2:xs)) myEnv = (evalBinaryOp op args1 args2 myEnv, myEnv)
evaluation (ListOfAst (If:cond:thens:elsee:xs)) myEnv = (evalCond cond thens elsee myEnv, myEnv)
evaluation (ListOfAst (Define:(Var (AstSymb (name))):value:xs)) myEnv = (Just (ListOfAst (Define:(Var (AstSymb (name))):value:xs)), pushEnv myEnv name value)
evaluation (ListOfAst []) myEnv = (Nothing, myEnv)

-- evaluation (Var (AstSymb name)) myEnv = (evalVarCall name myEnv, myEnv)

-- evaluation (Var n) myEnv = (Just (Var n), myEnv)

-- evaluation (Define name value) myEnv = (Just (Define name value), pushEnv myEnv name value)

-- evaluation (BinaryOp op args1 args2) myEnv = (evalBinaryOp op args1 args2 myEnv, myEnv)

-- evaluation (Cond args1 args2 args3) myEnv = (evalCond args1 args2 args3 myEnv, myEnv)

-- -- evaluation (FunctionCall name args) myEnv = (Just (FunctionCall name args), myEnv)

-- evaluation (FunctionCall name args) myEnv = (evalFunctionCall name args myEnv, myEnv)


evalVarCall :: String -> Env -> Maybe Ast
evalVarCall name myEnv = do
    case getEnv myEnv name of
        Just (ast) -> do
            let (result, _) =  (evaluation ast myEnv)
            result
        _ -> Nothing

evalFunctionCall :: String -> [Ast] -> Env -> Maybe Ast
evalFunctionCall name args myEnv = do
    case getEnv myEnv name of
        Just (ast) -> do
            let (result, _) =  (evaluation ast myEnv)
            result
        _ -> Nothing

evalCond :: Ast -> Ast -> Ast -> Env -> Maybe Ast
evalCond args1 args2 args3 myEnv = do
    case evaluation args1 myEnv of
        (Just (Var (AstBool "#t")), env1) -> do
            let (result, _) = evaluation args2 env1
            result
        (Just (Var (AstBool "#f")), env2) -> do
            let (result, _) = evaluation args3 env2
            result
        _ -> Nothing


evalBinaryOp :: AstBinaryOp -> Ast -> Ast -> Env -> Maybe Ast
evalBinaryOp Add ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstInt (n1 + n2)))
        _ -> Nothing

evalBinaryOp Multiply ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstInt (n1 * n2)))
        _ -> Nothing

evalBinaryOp Sub ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstInt (n1 - n2)))
        _ -> Nothing

evalBinaryOp Divide ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) ->
            if n2 /= 0 then Just (Var (AstInt (n1 `div` n2))) else Nothing
        _ -> Nothing

evalBinaryOp Modulo ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) ->
            if n2 /= 0 then Just (Var (AstInt (n1 `mod` n2))) else Nothing
        _ -> Nothing

evalBinaryOp Equal ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstBool (if n1 == n2 then "#t" else "#f")))
        _ -> Nothing

evalBinaryOp NotEqual ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstBool (if n1 /= n2 then "#t" else "#f")))
        _ -> Nothing

evalBinaryOp LessThan ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstBool (if n1 < n2 then "#t" else "#f")))
        _ -> Nothing

evalBinaryOp LessThanOrEqual ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstBool (if n1 <= n2 then "#t" else "#f")))
        _ -> Nothing

evalBinaryOp GreaterThan ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstBool (if n1 > n2 then "#t" else "#f")))
        _ -> Nothing

evalBinaryOp GreaterThanOrEqual ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstBool (if n1 >= n2 then "#t" else "#f")))
        _ -> Nothing

evalBinaryOp And ast1 ast2 myEnv = do
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstBool n1)), _), (Just (Var (AstBool n2)), _)) ->
            Just (Var (AstBool (if n1 == "#t" && n2 == "#t" then "#t" else "#f")))
        _ -> Nothing

evalBinaryOp Or ast1 ast2 myEnv =
    case (evaluation ast1 myEnv, evaluation ast2 myEnv) of
        ((Just (Var (AstBool n1)), _), (Just (Var (AstBool n2)), _)) ->
            (Just (Var (AstBool (if n1 == "#t" || n2 == "#t" then "#t" else "#f"))))
        _ -> Nothing
-- eval
-- BinaryOp _ _ _ = Nothing Obliger d'avoir Un opérateur

-- Save
-- evalBinaryOp LessThanOrEqual ast1 ast2 = do
--     case (evaluation ast1, evaluation ast2) of
--         ((Just (Var (AstInt n1)), _), (Just (Var (AstInt n2)), _)) -> Just (Var (AstBool (if n1 <= n2 then MyBool "#t" else MyBool "#f")))
--         _ -> Nothing

