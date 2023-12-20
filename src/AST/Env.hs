module AST.Env(
    module AST.Env
) where

import AST.Constants

data Env = Env
    { env :: [(String, Ast)]}
    deriving (Show, Eq)

createEnv :: Env
createEnv = Env []

pushEnv :: Env -> String -> Ast -> Env
pushEnv (Env actualEnv) name value =
    Env ([(name, value)] ++ actualEnv)

-- defineThing :: Env -> Ast -> Env
-- defineThing (Env existingEnv) (Define varName value) =
--     Env ((varName, value) : existingEnv)

getEnv :: Env -> String -> Maybe Ast
getEnv (Env []) _ = Nothing
getEnv (Env ((name, value) : xs)) varName =
    if name == varName then Just value else getEnv (Env xs) varName

printEnv :: Env -> IO ()
printEnv (Env existingEnv) = do
    putStrLn (show existingEnv)