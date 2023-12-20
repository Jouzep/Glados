module AST.Constants (
  module AST.Constants
) where
    
data SExpr
    = SInt Int
    | SSymb String
    | SList [SExpr]
    deriving (Show, Eq)

data Ast
    = Var AstConstant
    | FunctionCall String [Ast]
    | Define String Ast
    | Cond Ast Ast Ast
    | BinaryOp AstBinaryOp Ast Ast
    deriving (Show, Eq)

data AstConstant
    = AstInt Int
    -- | AstList [Ast]
    | AstSymb String
    | AstBool String
    deriving (Show, Eq)

-- data MyBool 
--     = #t
--     | #f
--     deriving (Show, Eq)

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