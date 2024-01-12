module VirtualMachine.StackMachine (exec, execOperator, execGetFunctionFromEnv, printResult) where

import VirtualMachine.VMConstants

exec :: Env -> Args -> Insts -> Stack -> Either String Value
exec env args [] [result] = Right result
exec env args (Push val : insts) stack = exec env args insts (val : stack)
exec env args (PushEnv funcName : insts) stack = do
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
exec env args (Ret : _) (result : _) = Right result
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
    | otherwise = exec env args  insts (IntVal (v1 `div` v2) : stack)
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
--     let program1 = [Push (IntVal 42), Ret]
--     let program2 = [Push (IntVal 0), Push (IntVal 41), Push (OpVal Divide1), Call, Ret]
--     let program3 = [Push (IntVal 10), Push (IntVal 10), Push (OpVal Eq1), Call, Ret]
--     let program4 = [Push (IntVal 2), Push (IntVal 5), Push (OpVal Less1), Call, Ret]
--     let program5 = [Push (IntVal 2), Push (IntVal 2), Push (OpVal Eq1), Call, JumpIfFalse (IntVal 1), Push (IntVal 1), Ret, Push (IntVal 4), Ret]
--     let program6 = [Push (IntVal 11), Push (IntVal 10), Push (OpVal Eq1), Call, JumpIfFalse (IntVal 2), Push (IntVal 1), Ret, Push (IntVal 2), Ret]
--     let programWithArgs = [PushArg 1, Push (IntVal 0), Push (OpVal Less1), Call, JumpIfFalse (IntVal 2), PushArg 1, Ret, PushArg 0, Push (IntVal (-1)), Push (OpVal Multiply1), Call, Ret]
--     let programWithUserFunc = [Push (IntVal (-42)), Push (FuncVal function), Call,Push (FuncVal function), Call, Push (IntVal 5), Push (OpVal Eq1), Call,JumpIfFalse (IntVal 2), Push (IntVal 1), Ret, Push (IntVal 2), Ret]
--     let env = [("fact", Push (FuncVal[PushArg 0, Push(IntVal 1), Push (OpVal Eq1), Call, JumpIfFalse (IntVal 2), Push (IntVal 1), Ret, Push (IntVal 1), PushArg 0, Push(OpVal Subtract1), Call, PushEnv "fact", Call, PushArg 0, Push (OpVal Multiply1), Call , Ret]))]
--                                  --   PushArg 0  Push 1          Push Eq          Call  JumpIfFalse 2           Push 1           Ret  Push 1           PushArg 0  Push Sub              Call  PushEnv "fact"  Call  PushArg 0  Push Mul               Call   Ret
--     let programWithEnvAndRecursive = [Push (IntVal 5), PushEnv "fact", Call, Ret]
--     putStrLn $ "test 1: " ++ printResult (exec [] args program1 [])
--     putStrLn $ "test 2: " ++ printResult (exec [] args program2 [])
--     putStrLn $ "test 3: " ++ printResult (exec [] args program3 [])
--     putStrLn $ "test 4: " ++ printResult (exec [] args program4 [])
--     putStrLn $ "test 5: " ++ printResult (exec [] args program5 [])
--     putStrLn $ "test 6: " ++ printResult (exec [] args program6 [])
--     putStrLn $ "test Args: " ++ printResult (exec [] args programWithArgs [])
--     putStrLn $ "test UserFunc: " ++ printResult (exec [] [] programWithUserFunc [])
--     putStrLn $ "test Env And Recursive Function: " ++ printResult (exec env [] programWithEnvAndRecursive [])

-- [0, -42]
-- [0, -42]

-- 0 < -42
-- False
-- [Mul, -1, -42, 0, -42]

-- main(a, b) {
--     func1(a b) {
--         let c = a + b
--         let a = func2(c) {
--             c vient de la stack ||  stack: [] args: []
--             let d = c + 1
--             let e = c + 2
--         }
--     }
-- }
