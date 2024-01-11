module VirtualMachine.VMConstants (Value(..),
Operator(..),
Instruction(..),
Stack(..),
Args(..),
Insts(..),
Env(..))where

data Value = IntVal Int | BoolVal Bool | FuncVal[Instruction] | OpVal Operator deriving (Show)

data Operator = Add1 | Subtract1 | Multiply1 | Divide1 | Eq1 | Less1 deriving (Show)

data Instruction = Push Value | Call | PushArg Int | PushEnv String | Ret | JumpIfFalse Value deriving (Show)

type Stack = [Value]
type Args = [Value]
type Insts = [Instruction]
type Env = [(String, Instruction)]