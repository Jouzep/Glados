module AST.Constants (Ast(..), AstBinaryOp(..), Func(..), Chiasse(..), Parser(..), CCond(..), Call(..)) where

-- PARSER

type Parser a = String -> Maybe (a, String)
type Func = ([String], [String], [Chiasse])
type Call = (String, [String])
type CCond = ([String], [Chiasse], [Chiasse])
data Chiasse
  = CFunction Func
  | CIf CCond
  | CCall Call
  | CNumber Int
  | CString String
  | CLine [String]
  deriving (Eq)

data Ast
    -- = Identifier AstIdentifier
    = Constant String
    | BinaryOp AstBinaryOp
    | FunctionCall String [Ast]
    | FunctionDefinition String [String] [Ast]
    | Conditional [String] [Ast] [Ast]
    | Define String [Ast]
    | Return [Ast]
    | ListAst [Ast]
    deriving (Eq)

instance Show Chiasse where
  show (CFunction (name, args, body)) = "CFunction " ++ show (name, args, body)
  show (CIf (condition, body, bodyElse)) = "CIf " ++ show (condition, body, bodyElse)
  show (CNumber n) = "CNumber " ++ show n
  show (CString s) = "CString " ++ show s
  show (CLine exprs) = "CLine " ++ show exprs
  show (CCall (name, args)) = "CCall " ++ show (name, args)

instance Show Ast where
--   show (Identifier identifier) = "Identifier " ++ show identifier
  show (Constant s) = show s
  show (BinaryOp binaryOp) = show binaryOp
  show (FunctionCall name args) = "FunctionCall " ++ show (name, args)
  show (FunctionDefinition name args body) = "FunctionDefinition " ++ show (name, args, body)
  show (Conditional cond bodyIf bodyElse) = "Conditional " ++ show (cond, bodyIf, bodyElse)
  show (Define str ast) = "Define " ++ show (str, ast)
  show (ListAst astList) = show astList
  show (Return value) = "Return " ++ show (value)

-- data AstIdentifier
--     = String
--     | Number
--     | Boolean
--     deriving (Show, Eq)

data ReturnIdentifier
    = ReturnString
    | ReturnNumber
    | ReturnBoolean
    deriving (Show, Eq)

data AstBinaryOp
    = Add
    | Multiply
    | Sub
    | Divide
    | Modulo
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | And
    | Or
    deriving (Show, Eq)