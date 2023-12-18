module Constants(SExpr, Ast) where

data SExpr
    = SInt Int
    | SSymb String
    | SList [SExpr]
    deriving Show

data Ast
    = Var AstConstant
    | FunctionCall String [Ast]
    | Define String Ast
    | Cond Ast Ast Ast
    | BinaryOp AstBinaryOp Ast Ast
    deriving Show

data AstConstant
    = AstInt Int
    | AstSymb String
    | AstBool Bool
    deriving Show

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
    deriving Show
