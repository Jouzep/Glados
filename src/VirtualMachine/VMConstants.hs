module VirtualMachine.VMConstants (Value(..),
Operator(..),
Instruction(..),
Stack(..),
Args(..),
Insts(..),
Env(..))where

data Value = StringVal String | IntVal Int | BoolVal Bool | FuncVal[Instruction] | OpVal Operator deriving (Show, Eq)

data Operator = Add1 | Subtract1 | Multiply1 | Divide1 | Modulo1 | Eq1 | Less1 deriving (Show, Eq)

data Instruction = Push Value | Call | PushArg Int | PushEnv String | Ret | JumpIfFalse Value deriving (Show, Eq)

type Stack = [Value]
type Args = [Value]
type Insts = [Instruction]
type Env = [(String, Instruction)]
