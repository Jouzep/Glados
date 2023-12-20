module AST.Constants (SExpr(..), Ast(..), AstConstant(..), AstBinaryOp(..)) where

data SExpr
    = SInt Int
    | SSymb String
    | SList [SExpr]
    deriving (Show, Eq)

data Ast
    = Var AstConstant
    | Define
    | If
    | BinaryOp AstBinaryOp
    | Func Ast Ast
    | FunctionCall
    | Lambda
    | ListOfAst [Ast]
    deriving (Show, Eq)

data AstConstant
    = AstInt Int
    -- | AstList [Ast]
    | AstSymb String
    | AstBool String
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
