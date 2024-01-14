module AST.ConvertAstInsts (convertAstToInsts) where

import AST.Constants
import VirtualMachine.VMConstants
import Data.Char (isDigit)

convertAstToInsts :: Ast -> VirtualMachine.VMConstants.Env -> (Insts, VirtualMachine.VMConstants.Env)
convertAstToInsts (Define name value) env list = ([PushEnv name, Call], (name, Push (convertAstToValue value)) : env)
convertAstToInsts (FunctionDefinition name args body) env list =
  ([PushEnv name, Call], (name, Push (FuncVal (fst (convertAstToInsts (ListAst body) env)))) : env)
convertAstToInsts (ListAst asts) env list =
  foldl (\(accInsts, accEnv) ast -> let (insts, newEnv) = convertAstToInsts ast accEnv in (accInsts ++ insts, newEnv)) ([], env) asts
convertAstToInsts ast env list = (convertAstToInsts' ast list, env)

convertAstToInsts' :: Ast -> Insts -> Insts
convertAstToInsts' (Identifier a) list = list ++ [Push (convertIdentifierToValue a)]
convertAstToInsts' (BinaryOp op) list = list ++ [Push (OpVal (convertBinaryOpToOperator op)), Call]
convertAstToInsts' (FunctionCall name body) list = list ++ [PushEnv a, Call]
convertAstToInsts' _ list = list  -- Default case for other Ast types

convertIdentifierToValue :: String -> Value
convertIdentifierToValue s
  | all isDigit s = IntVal (read s)
  | otherwise = StringVal s
  where
    isDigit c = c `elem` "0123456789"

convertBoolToBoolVal :: String -> Bool
convertBoolToBoolVal "#t" = True
convertBoolToBoolVal "#f" = False

convertBinaryOpToOperator :: AstBinaryOp -> Operator
convertBinaryOpToOperator Add = Add1
convertBinaryOpToOperator Multiply = Multiply1
convertBinaryOpToOperator Sub = Subtract1
convertBinaryOpToOperator Divide = Divide1
convertBinaryOpToOperator Equal = Eq1
convertBinaryOpToOperator LessThan = Less1
