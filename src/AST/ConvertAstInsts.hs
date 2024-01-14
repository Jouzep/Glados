module AST.ConvertAstInsts (convertAstToInsts) where

import AST.Constants
import AST.Env
import VirtualMachine.VMConstants

convertAstToInsts :: Ast -> VirtualMachine.VMConstants.Env -> (Insts, VirtualMachine.VMConstants.Env)
convertAstToInsts (Define, Var (AstSymb a)) env = ([PushEnv a, Call], env)  -- Example for AstSymb
convertAstToInsts (Define, Var (AstFunc a)) env = ([PushEnv a, Call], env)  -- Example for AstFunc
-- convertAstToInsts (Define, Var a) env = ([PushEnv (convertAstconstToValue a), Call], env)  -- Example for other Var cases
convertAstToInsts (Var a) env = ([Push (convertAstconstToValue a)], env)
convertAstToInsts (BinaryOp op) env = ([Push (OpVal (convertBinaryOpToOperator op)), Call], env)
-- convertAstToInsts (FunctionCall a) env = ([PushEnv a, Call], env)
convertAstToInsts (ListOfAst asts) env = foldl (\(accInsts, accEnv) ast -> let (insts, newEnv) = convertAstToInsts ast accEnv in (accInsts ++ insts, newEnv)) ([], env) asts

convertAstconstToValue :: AstConstant -> Value
convertAstconstToValue (AstInt a) = IntVal a
convertAstconstToValue (AstBool a) = BoolVal (convertBoolToBoolVal a)

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
