data Value = IntVal Int | BoolVal Bool deriving (Show)

data Operator = Add | Subtract | Multiply | Divide | Eq | Less deriving (Show)

data Instruction = Push Value | Call Operator | Ret | JumpIfFalse deriving (Show)

type Stack = [Value]
type Insts = [Instruction]

exec :: Insts -> Stack -> Either String Value
exec [] [result] = Right result
exec (Push val : insts) stack = exec insts (val : stack)
exec (Call op : insts) stack = do
    case stack of 
        (IntVal v2 : (IntVal v1 : rest)) -> execOperator op insts stack
        _ -> Left $ "Error: " ++ show op ++ "Operator call needs two integer arguments"
exec (JumpIfFalse : insts) (BoolVal False : stack) = exec insts stack
exec (Ret : _) [result] = Right result
exec _ _ = Left "Invalid operation or insufficient operands"


execOperator :: Operator -> Insts -> Stack -> Either String Value
execOperator Add insts (IntVal v1 : IntVal v2 : stack) = exec insts (IntVal (v1 + v2) : stack)
execOperator Subtract insts (IntVal v1 : IntVal v2 : stack) = exec insts (IntVal (v1 - v2) : stack)
execOperator Multiply insts (IntVal v1 : IntVal v2 : stack) = exec insts (IntVal (v1 * v2) : stack)
execOperator Divide insts (IntVal v1 : IntVal v2 : stack)
    | v2 == 0 = Left "Error: division by zero"
    | otherwise = exec insts (IntVal (v1 `div` v2) : stack)
execOperator Eq insts (IntVal v1 : IntVal v2 : stack) = exec insts (BoolVal (v1 == v2) : stack)
execOperator Less insts (IntVal v1 : IntVal v2 : stack) = exec insts (BoolVal (v1 < v2) : stack)

printResult :: Either String Value -> String
printResult (Right (BoolVal val)) = show val
printResult (Right (IntVal val)) = show val
printResult (Left errMsg) = errMsg

main :: IO ()
main = do
    let program1 = [Push (IntVal 42), Ret]
    let program2 = [Push (IntVal 0), Push (IntVal 41), Call Divide, Ret]
    let program3 = [Push (IntVal 10), Push (IntVal 10), Call Eq, Ret]
    let program4 = [Push (IntVal 2), Push (IntVal 5), Call Less, Ret]
    let program5 = [Push (IntVal 2), Push (IntVal 5), Call Eq, JumpIfFalse, Push (IntVal 1), Ret, Push (IntVal 2), Ret]
    putStrLn $ printResult (exec program1 [])
    putStrLn $ printResult (exec program2 [])
    putStrLn $ printResult (exec program3 [])
    putStrLn $ printResult (exec program4 [])
    putStrLn $ printResult (exec program5 [])