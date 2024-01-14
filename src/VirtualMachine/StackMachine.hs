module VirtualMachine.StackMachine (exec, execOperator, execGetFunctionFromEnv, printResult, printChiasseResult) where

import VirtualMachine.VMConstants

exec :: Env -> Args -> Insts -> Stack -> Either String Value
exec env args [] [result] = Right result
exec env args (Push val : insts) stack = exec env args insts (val : stack)
exec env args (PushEnv funcName : insts) stack = do
    -- putStrLn $ "result from main function"
    case (execGetFunctionFromEnv env funcName) of
        Right inst -> exec env args (inst: insts) stack
        Left error -> Left error
exec env args (Call : insts) (FuncVal f : stack) = do
    case (exec env stack f []) of
        Right returnValue -> exec env args insts (returnValue : stack)
        Left error -> Left error
exec env args (Call : insts) (OpVal op : stack) = execOperator env args (OpVal op) insts stack
exec env args (Call : _) _ = Left "Call instruction requires a function or operator on the stack"
exec env args (JumpIfFalse (IntVal number) : insts) (BoolVal False : stack) = exec env args (drop number insts) stack
exec env args (JumpIfFalse (IntVal number) : insts) (BoolVal True : stack) = exec env args (insts) stack
exec env args (JumpIfFalse (IntVal number) : insts) (_ : stack) = Left "Conditions requires a boolean on the stack"
exec env args (Ret : _) (result : _) = Right result
exec env args (Ret : _) _ = Left "Return need a value on the stack"
exec env args (PushArg index : insts) stack
    | index < length args = exec env args insts ((args !! index) : stack)
    | otherwise = Left $ "Invalid argument index: " ++ show index
exec env args insts stacks = Left $ "Invalid operation or insufficient operands : Instruction:  " ++ show insts ++ "   STACK :  "++ show stacks

execGetFunctionFromEnv :: Env -> String -> Either String Instruction
execGetFunctionFromEnv [] funcName = Left $ "Function " ++ funcName ++ " not found"
execGetFunctionFromEnv ((name, inst) : env) funcName
    | name == funcName = Right inst
    | otherwise = execGetFunctionFromEnv env funcName

execOperator :: Env -> Args -> Value -> Insts -> Stack -> Either String Value
execOperator env args (OpVal Add1) insts (IntVal v1 : IntVal v2 : stack) = exec env args  insts (IntVal (v1 + v2) : stack)
execOperator env args (OpVal Subtract1) insts (IntVal v1 : IntVal v2 : stack) = exec env args  insts (IntVal (v1 - v2) : stack)
execOperator env args (OpVal Multiply1) insts (IntVal v1 : IntVal v2 : stack) = exec env args  insts (IntVal (v1 * v2) : stack)
execOperator env args (OpVal Divide1) insts (IntVal v1 : IntVal v2 : stack)
    | v2 == 0 = Left "Error: division by zero"
    | otherwise = exec env args insts (IntVal (v1 `div` v2) : stack)
execOperator env args (OpVal Modulo1) insts (IntVal v1 : IntVal v2 : stack)
    | v2 == 0 = Left "Error: division by zero"
    | otherwise = exec env args insts (IntVal (v1 `mod` v2) : stack)
execOperator env args (OpVal Eq1) insts (IntVal v1 : IntVal v2 : stack) = exec env args  insts (BoolVal (v1 == v2) : stack)
execOperator env args (OpVal Less1) insts (IntVal v1 : IntVal v2 : stack) = exec env args  insts (BoolVal (v1 < v2) : stack)

printResult :: Either String Value -> String
printResult (Right (BoolVal val)) = show val
printResult (Right (IntVal val)) = show val
printResult (Left errMsg) = errMsg

-- main :: IO ()
-- main = do
--     let args = [IntVal 42, IntVal (-42)]
--     let function = [PushArg 0, Push (IntVal 0), Push (OpVal Less1), Call, JumpIfFalse (IntVal 2), PushArg 0, Ret, PushArg 0, Push (IntVal (-1)), Push (OpVal Multiply1), Call, Ret]
--     let env = [("fact", Push (FuncVal[PushArg 0, Push(IntVal 1), Push (OpVal Eq1), Call, JumpIfFalse (IntVal 2), Push (IntVal 1), Ret, Push (IntVal 1), PushArg 0, Push(OpVal Subtract1), Call, PushEnv "fact", Call, PushArg 0, Push (OpVal Multiply1), Call , Ret]))]
--     let programDivide1 = [Push (IntVal 0), Push (IntVal 42), Push (OpVal Divide1), Call, Ret]
--     let programDivide2 = [Push (IntVal 2), Push (IntVal 42), Push (OpVal Divide1), Call, Ret]
--     let programModulo1 = [Push (IntVal 2), Push (IntVal 5), Push (OpVal Modulo1), Call, Ret]
--     let programModulo2 = [Push (IntVal 0), Push (IntVal 9), Push (OpVal Modulo1), Call, Ret]
--     let programMultiply = [Push (IntVal 3), Push (IntVal 10), Push (OpVal Multiply1), Call, Ret]
--     let programEqual1 = [Push (IntVal 10), Push (IntVal 10), Push (OpVal Eq1), Call, Ret]
--     let programEqual2 = [Push (IntVal 10), Push (IntVal 11), Push (OpVal Eq1), Call, Ret]
--     let programLess1 = [Push (IntVal 2), Push (IntVal 5), Push (OpVal Less1), Call, Ret]
--     let programLess2 = [Push (IntVal 5), Push (IntVal 2), Push (OpVal Less1), Call, Ret]
--     let programEq1 = [Push (IntVal 5), Push (IntVal 5), Push (OpVal Eq1), Call, Ret]
--     let programEq2 = [Push (IntVal 2), Push (IntVal 5), Push (OpVal Eq1), Call, Ret]
--     let programWithArgs1 = [PushArg 1, Ret]
--     let programWithArgs2 = [PushArg 3, Ret]
--     let programWithUserFunc = [Push (IntVal (-42)), Push (FuncVal function), Call, Push (OpVal Eq1), Call, Ret]
--     let programWithEnvAndRecursive = [Push (IntVal 5), PushEnv "fact", Call, Ret]
--     let programRet1 = [Push (IntVal 42), Ret]
--     let programRet2 = [Ret]
--     let programTestJumpIfFalse1 = [Push (IntVal 5), Push (IntVal 5), JumpIfFalse (IntVal 1), Push (IntVal 3), Ret]
--     let programTestJumpIfFalse2 = [Push (IntVal 5), Push (IntVal 5), Push (OpVal Eq1), Call, JumpIfFalse (IntVal 2), Push (IntVal 3), Ret, Push (IntVal 1), Ret]
--     let programTestJumpIfFalse3 = [Push (IntVal 5), Push (IntVal 6), Push (OpVal Eq1), Call, JumpIfFalse (IntVal 2), Push (IntVal 3), Ret, Push (IntVal 1), Ret]
--     putStrLn $ "test Divide 42 / 0: \n" ++ printResult (exec [] args programDivide1 []) ++ "\n"
--     putStrLn $ "test Divide 42 / 2: \n" ++ printResult (exec [] args programDivide2 []) ++ "\n"
--     putStrLn $ "test Modulo 5 % 2:\n" ++ printResult (exec [] args programModulo1 []) ++ "\n"
--     putStrLn $ "test Modulo 9 % 0:\n" ++ printResult (exec [] args programModulo2 []) ++ "\n"
--     putStrLn $ "test Multiply 3 * 10: \n" ++ printResult (exec [] args programMultiply []) ++ "\n"
--     putStrLn $ "test Equal true: \n" ++ printResult (exec [] args programEqual1 []) ++ "\n"
--     putStrLn $ "test Equal false: \n" ++ printResult (exec [] args programEqual2 []) ++ "\n"
--     putStrLn $ "test Less: 5 < 2: \n" ++ printResult (exec [] args programLess1 []) ++ "\n"
--     putStrLn $ "test Less: 2 < 5:\n" ++ printResult (exec [] args programLess2 []) ++ "\n"
--     putStrLn $ "test Equal true:\n" ++ printResult (exec [] args programEq1 []) ++ "\n"
--     putStrLn $ "test Equal false:\n" ++ printResult (exec [] args programEq2 []) ++ "\n"
--     putStrLn $ "test Return Ok: \n" ++ printResult (exec [] args programRet1 []) ++ "\n"
--     putStrLn $ "test Return error empty stack: \n" ++ printResult (exec [] args programRet2 []) ++ "\n"
--     putStrLn $ "test JumpIfFalse should error no boolean on stack: \n" ++ printResult (exec [] [] programTestJumpIfFalse1 []) ++ "\n"
--     putStrLn $ "test JumpIfFalse should return 3: \n" ++ printResult (exec [] [] programTestJumpIfFalse2 []) ++ "\n"
--     putStrLn $ "test JumpIfFalse should return 1: \n" ++ printResult (exec [] [] programTestJumpIfFalse3 []) ++ "\n"
--     putStrLn $ "test Args ok: \n" ++ printResult (exec [] args programWithArgs1 []) ++ "\n"
--     putStrLn $ "test Args index error: \n" ++ printResult (exec [] args programWithArgs2 []) ++ "\n"
--     putStrLn $ "test UserFunc: \n" ++ printResult (exec [] [] programWithUserFunc []) ++ "\n"
--     putStrLn $ "test Env And Recursive Function: \n" ++ printResult (exec env [] programWithEnvAndRecursive []) ++ "\n"

printChiasseResult :: Value -> IO ()
printChiasseResult ((IntVal val)) = putStrLn (show val)