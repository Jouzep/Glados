data Value = IntVal Int | BoolVal Bool | FuncVal[Instruction] | OpVal Operator deriving (Show)

data Operator = Add | Subtract | Multiply | Divide | Eq | Less deriving (Show)

data Instruction = Push Value | Call | PushArg Int | Ret | JumpIfFalse Value deriving (Show)

type Stack = [Value]
type Args = [Value]
type Insts = [Instruction]

exec :: Args -> Insts -> Stack -> Either String Value
exec args [] [result] = Right result
exec args (Push val : insts) stack = exec args insts (val : stack)
exec args (Call : insts) (FuncVal f : stack) = do
    case (exec stack f []) of
        Right returnValue -> exec args insts (returnValue : stack)
        Left error -> Left error
    -- exec args insts ((exec stack f []): stack) -- result of the function 
exec args (Call : insts) (OpVal op : stack) = execOperator args (OpVal op) insts stack
exec args (Call : _) _ = Left "Call instruction requires a function or operator on the stack"
exec args (JumpIfFalse (IntVal number) : insts) (BoolVal False : stack) = exec args (drop number insts) stack
exec args (JumpIfFalse (IntVal number) : insts) (BoolVal True : stack) = exec args (insts) stack
exec args (Ret : _) (result : _) = Right result
exec args (PushArg index : insts) stack
    | index < length args = exec args insts ((args !! index) : stack)
    | otherwise = Left $ "Invalid argument index: " ++ show index
exec args insts stacks = Left $ "Invalid operation or insufficient operands : Instruction:  " ++ show insts ++ "   STACK :  "++ show stacks


execOperator :: Args -> Value -> Insts -> Stack -> Either String Value
execOperator args (OpVal Add) insts (IntVal v1 : IntVal v2 : stack) = exec args insts (IntVal (v1 + v2) : stack)
execOperator args (OpVal Subtract) insts (IntVal v1 : IntVal v2 : stack) = exec args insts (IntVal (v1 - v2) : stack)
execOperator args (OpVal Multiply) insts (IntVal v1 : IntVal v2 : stack) = exec args insts (IntVal (v1 * v2) : stack)
execOperator args (OpVal Divide) insts (IntVal v1 : IntVal v2 : stack)
    | v2 == 0 = Left "Error: division by zero"
    | otherwise = exec args insts (IntVal (v1 `div` v2) : stack)
execOperator args (OpVal Eq) insts (IntVal v1 : IntVal v2 : stack) = exec args insts (BoolVal (v1 == v2) : stack)
execOperator args (OpVal Less) insts (IntVal v1 : IntVal v2 : stack) = exec args insts (BoolVal (v1 < v2) : stack)

printResult :: Either String Value -> String
printResult (Right (BoolVal val)) = show val
printResult (Right (IntVal val)) = show val
printResult (Left errMsg) = errMsg

main :: IO ()
main = do
    let args = [IntVal 42, IntVal (-42)]
    let function = [PushArg 0, Push (IntVal 0), Push (OpVal Less), Call, JumpIfFalse (IntVal 2), PushArg 0, Ret, PushArg 0, Push (IntVal (-1)), Push (OpVal Multiply), Call, Ret]
    let program1 = [Push (IntVal 42), Ret]
    let program2 = [Push (IntVal 0), Push (IntVal 41), Push (OpVal Divide), Call, Ret]
    let program3 = [Push (IntVal 10), Push (IntVal 10), Push (OpVal Eq), Call, Ret]
    let program4 = [Push (IntVal 2), Push (IntVal 5), Push (OpVal Less), Call, Ret]
    let program5 = [Push (IntVal 2), Push (IntVal 2), Push (OpVal Eq), Call, JumpIfFalse (IntVal 1), Push (IntVal 1), Ret, Push (IntVal 4), Ret]
    let program6 = [Push (IntVal 11), Push (IntVal 10), Push (OpVal Eq), Call, JumpIfFalse (IntVal 2), Push (IntVal 1), Ret, Push (IntVal 2), Ret]
    let programWithArgs = [PushArg 1, Push (IntVal 0), Push (OpVal Less), Call, JumpIfFalse (IntVal 2), PushArg 1, Ret, PushArg 0, Push (IntVal (-1)), Push (OpVal Multiply), Call, Ret]
    let programWithUserFunc = [Push (IntVal (-42)), Push (FuncVal function), Call,Push (FuncVal function), Call, Push (IntVal 5), Push (OpVal Eq), Call,JumpIfFalse (IntVal 2), Push (IntVal 1), Ret, Push (IntVal 2), Ret]

    putStrLn $ "test 1: " ++ printResult (exec args program1 [])
    putStrLn $ "test 2: " ++ printResult (exec args program2 [])
    putStrLn $ "test 3: " ++ printResult (exec args program3 [])
    putStrLn $ "test 4: " ++ printResult (exec args program4 [])
    putStrLn $ "test 5: " ++ printResult (exec args program5 [])
    putStrLn $ "test 6: " ++ printResult (exec args program6 [])
    putStrLn $ "test Args: " ++ printResult (exec args programWithArgs [])
    putStrLn $ "test UserFunc: " ++ printResult (exec [] programWithUserFunc [])



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
