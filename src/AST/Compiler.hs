module AST.Compiler (compiler) where

import AST.Constants
import VirtualMachine.VMConstants
import Data.Char (isDigit)

compiler :: [Ast] -> Insts -> VirtualMachine.VMConstants.Env -> Args -> Maybe (Insts, VirtualMachine.VMConstants.Env)
compiler asts insts env args = case compilerAst insts asts env of
    Just (insts', env', asts') -> case compiler asts' insts env' args of
        Just (insts'', env'') -> Just (insts'', env'')
        Nothing -> Just (insts', env')
    Nothing -> Nothing
compiler _ insts env args = Just (insts, env)

findReturn :: [Ast] -> VirtualMachine.VMConstants.Env -> Maybe (VirtualMachine.VMConstants.Env, Insts)
findReturn [] _ = Nothing
findReturn ((Return ((Constant x):xxsm):xs)) env = Just (env, [Push (convertStringToValue x)])
findReturn (x:xs) env = findReturn xs env


compilerAst :: Insts -> [Ast] -> VirtualMachine.VMConstants.Env -> Maybe (Insts, VirtualMachine.VMConstants.Env, [Ast])
compilerAst insts ((Define name body):xs) env = Just (insts, env ++ [(name, compilerBody insts body env)], xs)
compilerAst insts ((FunctionDefinition name args body):xs) env = case findReturn body env of
    Just (_, _) -> Just (insts, env ++ [(name, Push (FuncVal (map (\x -> compilerBody insts [x] env) body ++ [Ret])))] , xs)
    Nothing -> Just (insts, env ++ [(name, Push (FuncVal (map (\x -> compilerBody insts [x] env) body ++ [Call] ++ [Ret])))] , xs)
compilerAst insts ((FunctionCall name args):xs) env = Just (insts ++ [PushEnv name] ++ [Call] ++ [Ret], env, xs)
compilerAst _ _ _ = Nothing

compilerBody :: Insts -> [Ast] -> VirtualMachine.VMConstants.Env -> Instruction
compilerBody _ ((Constant str):xs) env = Push (convertStringToValue str)
compilerBody insts ((FunctionCall "add" [Constant x, Constant y]):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) [Constant y, Constant x] ++ [Push (OpVal Add1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "add" args):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) args ++ [Call] ++ [Push (OpVal Add1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "sub" [Constant x, Constant y]):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) [Constant y, Constant x] ++ [Push (OpVal Subtract1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "sub" args):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) args ++ [Call] ++ [Push (OpVal Subtract1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "divide" [Constant x, Constant y]):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) [Constant y, Constant x] ++ [Push (OpVal Divide1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "divide" args):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) args ++ [Call] ++ [Push (OpVal Divide1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "multiply" [Constant x, Constant y]):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) [Constant y, Constant x] ++ [Push (OpVal Multiply1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "multiply" args):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) args ++ [Call] ++ [Push (OpVal Multiply1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "modulo" [Constant x, Constant y]):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) [Constant y, Constant x] ++ [Push (OpVal Modulo1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall "modulo" args):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) args ++ [Push (OpVal Modulo1)] ++ [Call] ++ [Ret]))
compilerBody insts ((FunctionCall name args):xs) env = Push (FuncVal (map (\x -> compilerBody insts [x] env) args ++ [Call] ++ [Ret]))
compilerBody insts ((Define name body):xs) env = case compilerAst insts [Define name body] env of
            Just (_, (name, inst) : env, xs) -> inst
            Nothing -> Push (FuncVal [])
compilerBody insts ((Return value):xs) env = compilerBody insts value env

convertStringToValue :: String -> Value
convertStringToValue s
  | all isDigit s = IntVal (read s)
  | s == "true" = BoolVal True
  | s == "false" = BoolVal False
  | Just op <- getOperator s = OpVal op
  | otherwise = StringVal s
  where
    isDigit c = c `elem` "0123456789"

getOperator :: String -> Maybe Operator
getOperator s = case convertBinaryOpToOperator s of
  op@(Add1 {}) -> Just op
  op@(Subtract1 {}) -> Just op
  op@(Multiply1 {}) -> Just op
  op@(Divide1 {}) -> Just op
  op@(Modulo1 {}) -> Just op
  op@(Eq1 {}) -> Just op
  op@(Less1 {}) -> Just op
  _ -> Nothing


convertBinaryOpToOperator :: String -> Operator
convertBinaryOpToOperator "+" = Add1
convertBinaryOpToOperator "-" = Subtract1
convertBinaryOpToOperator "*" = Multiply1
convertBinaryOpToOperator "/" = Divide1
convertBinaryOpToOperator "%" = Modulo1
convertBinaryOpToOperator "==" = Eq1
convertBinaryOpToOperator "<" = Less1
