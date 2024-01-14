module AST.Compiler (compiler) where

import AST.Constants
import VirtualMachine.VMConstants
import Data.Char (isDigit)

compiler :: [Ast] -> Insts -> VirtualMachine.VMConstants.Env -> Args -> (Insts, VirtualMachine.VMConstants.Env)
compiler [(Define name body)] insts env args = (insts, compilerDefine name body env)
compiler (FunctionCall name args :rest) insts env _ = compiler rest (insts ++ [PushEnv name] ++ compilerArgs args env ++ [Call]) env args

compilerDefine :: String -> [Ast] -> VirtualMachine.VMConstants.Env -> VirtualMachine.VMConstants.Env
compilerDefine name body env = env ++ [(name, compilerBody body env)]

compilerBody :: [Ast] -> VirtualMachine.VMConstants.Env -> Instruction
compilerBody (firstElement : rest) env =
    case firstElement of
        Constant str -> Push (convertStringToValue str)

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
