data Value = IntVal Int | BoolVal Bool deriving (Show)

data Operator = Add | Subtract | Multiply | Divide | Eq | Less deriving (Show)

data Instruction = Push Value | Call Operator | PushArg Int | Ret | JumpIfFalse Value deriving (Show)

type Stack = [Value]
type Args = [Value]
type Insts = [Instruction]

exec :: Args -> Insts -> Stack -> Either String Value
exec args [] [result] = Right result
exec args (Push val : insts) stack = exec args insts (val : stack)
exec args (Call op : insts) stack = do
    case stack of
        (IntVal v2 : (IntVal v1 : rest)) -> execOperator args op insts stack
        _ -> Left $ "Error: " ++ show op ++ "Operator call needs two integer arguments"
exec args (JumpIfFalse (IntVal number) : insts) (BoolVal False : stack) = exec args (drop number insts) stack
exec args (JumpIfFalse (IntVal number) : insts) (BoolVal True : stack) = exec args (insts) stack
exec args (Ret : _) [result] = Right result
exec args (PushArg index : insts) stack
    | index < length args = exec args insts ((args !! index) : stack)
    | otherwise = Left $ "Invalid argument index: " ++ show index

exec args insts stacks = Left $ "Invalid operation or insufficient operands : " ++ show insts ++ show stacks


execOperator :: Args -> Operator -> Insts -> Stack -> Either String Value
execOperator args Add insts (IntVal v1 : IntVal v2 : stack) = exec args insts (IntVal (v1 + v2) : stack)
execOperator args Subtract insts (IntVal v1 : IntVal v2 : stack) = exec args insts (IntVal (v1 - v2) : stack)
execOperator args Multiply insts (IntVal v1 : IntVal v2 : stack) = exec args insts (IntVal (v1 * v2) : stack)
execOperator args Divide insts (IntVal v1 : IntVal v2 : stack)
    | v2 == 0 = Left "Error: division by zero"
    | otherwise = exec args insts (IntVal (v1 `div` v2) : stack)
execOperator args Eq insts (IntVal v1 : IntVal v2 : stack) = exec args insts (BoolVal (v1 == v2) : stack)
execOperator args Less insts (IntVal v1 : IntVal v2 : stack) = exec args insts (BoolVal (v1 < v2) : stack)

printResult :: Either String Value -> String
printResult (Right (BoolVal val)) = show val
printResult (Right (IntVal val)) = show val
printResult (Left errMsg) = errMsg

main :: IO ()
main = do
    let args = [IntVal 42, IntVal (-42)]
    let program1 = [Push (IntVal 42), Ret]
    let program2 = [Push (IntVal 0), Push (IntVal 41), Call Divide, Ret]
    let program3 = [Push (IntVal 10), Push (IntVal 10), Call Eq, Ret]
    let program4 = [Push (IntVal 2), Push (IntVal 5), Call Less, Ret]
    let program5 = [Push (IntVal 2), Push (IntVal 2), Call Eq, JumpIfFalse (IntVal 1), Push (IntVal 1), Ret, Push (IntVal 4), Ret]
    let program6 = [Push (IntVal 11), Push (IntVal 10), Call Eq, JumpIfFalse (IntVal 2), Push (IntVal 1), Ret, Push (IntVal 2), Ret]
    let programWithArgs = [PushArg 1, Push (IntVal 0), Call Less, JumpIfFalse (IntVal 2), PushArg 1, Ret, PushArg 0, Push (IntVal (-1)), Call Multiply, Ret]
    putStrLn $ printResult (exec args program1 [])
    putStrLn $ printResult (exec args program2 [])
    putStrLn $ printResult (exec args program3 [])
    putStrLn $ printResult (exec args program4 [])
    putStrLn $ printResult (exec args program5 [])
    putStrLn $ printResult (exec args program6 [])
    putStrLn $ printResult (exec args programWithArgs [])
