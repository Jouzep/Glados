-- Evaluation.hs
module VirtualMachine.Evaluation (evaluation) where

import AST.Constants
import AST.Env

evaluation :: Ast -> Env -> (Maybe Ast, Maybe Env)
evaluation (Var (AstSymb n)) myEnv = ((evalVarCall n myEnv), Just myEnv)
evaluation (Var n) myEnv = (Just (Var n), Just myEnv)

-- Operation
evaluation (ListOfAst [BinaryOp op,args1,(args2)]) myEnv = (evalBinaryOp op args1 (args2) myEnv, Just myEnv)

-- Condition
evaluation (ListOfAst [If,cond,thens,elsee]) myEnv = (evalCond cond thens elsee myEnv, Just myEnv)

-- Define
-- evaluation (ListOfAst (Define:(Var (AstSymb (name))):value:xs)) myEnv = (Just (ListOfAst (Define:(Var (AstSymb (name))):value:xs)), evalDefineVar name value myEnv)
evaluation (ListOfAst (Define:ListOfAst ((Var (AstSymb (name))):args):ListOfAst function:xs)) myEnv = (Just (ListOfAst (Define:ListOfAst ((Var (AstSymb (name))):args):ListOfAst function:xs)), evalDefineFunction name args function myEnv)
evaluation (ListOfAst (Define:(Var (AstSymb (name))):value:xs)) myEnv = (Just (ListOfAst (Define:(Var (AstSymb (name))):value:xs)), Just (pushEnv myEnv name value))
-- FunctionCall
-- evaluation (ListOfAst((Var (AstSymb (name)):rest)) myEnv = (evalFunctionCall name (ListOfAst rest), Just myEnv)
evaluation (ListOfAst (Var (AstSymb name) : rest)) myEnv = (evalFunctionCall name (ListOfAst rest) myEnv, Just myEnv)
evaluation (ListOfAst (ListOfAst(Lambda:params:body:values):xs)) myEnv = (evalLambda params body (ListOfAst values) myEnv, Just myEnv)
--                                       AST : AST : [AST]
evaluation _ _= (Nothing, Nothing)

replaceElements :: Eq a => [(a, a)] -> [a] -> [a]
replaceElements replacementsList defaultList = map replace defaultList
  where
    replace x = case lookup x replacementsList of
      Just newValue -> newValue
      Nothing -> x
evalLambda :: Ast -> Ast -> Ast -> Env -> Maybe Ast
evalLambda (ListOfAst params) (ListOfAst body) (ListOfAst values) myEnv = do
    let replacedValue = replaceElements (zip params values) body
    let (result, _) =  (evaluation (ListOfAst replacedValue) myEnv)
    result
evalLambda _ _ _ _ = Nothing

evalFunctionCall :: String -> Ast -> Env -> Maybe Ast
evalFunctionCall name (ListOfAst args) myEnv = do
    case getEnv myEnv name of
        Just (ListOfAst [ListOfAst params, ListOfAst body]) -> do
            let replacedValue = replaceElements (zip params args) body
            let (result, _) =  (evaluation (ListOfAst replacedValue) myEnv)
            result
        Just (ListOfAst [Lambda, ListOfAst params, ListOfAst body]) -> do
            let replacedValue = replaceElements (zip params args) body
            let (result, _) =  (evaluation (ListOfAst replacedValue) myEnv)
            result
        _ -> Nothing
evalFunctionCall _ _ _ = Nothing

evalDefineFunction :: String -> [Ast] -> [Ast] -> Env -> Maybe Env
evalDefineFunction name args functions myEnv =
    Just (pushEnv myEnv name (ListOfAst [ListOfAst args, ListOfAst functions]))

evalDefineVar :: String -> Ast -> Env -> Maybe Env
evalDefineVar name value myEnv = do
    case evaluation value myEnv of
        (Just newValue, Just newEnv) -> Just (pushEnv newEnv name newValue)
        _ -> Nothing

evalVarCall :: String -> Env -> Maybe Ast
evalVarCall name myEnv = do
    case getEnv myEnv name of
        Just (ast) -> do
            let (result, _) =  (evaluation ast myEnv)
            result
        _ -> Nothing


evalCond :: Ast -> Ast -> Ast -> Env -> Maybe Ast
evalCond args1 args2 args3 myEnv = do
    case evaluation args1 myEnv of
        (Just (Var (AstBool "#t")), Just env1) -> do
            let (result, _) = evaluation args2 env1
            result
        (Just (Var (AstBool "#f")), Just env2) -> do
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

