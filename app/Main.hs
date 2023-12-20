module Main (main) where

import Control.Exception (catch, IOException)
import AST.Constants()
import Parser.Parser
import AST.Tree.ConvertToAst
import Evaluation.Evaluation
import AST.Env
import Lib()

processInput :: String -> AST.Env.Env -> IO ()
processInput "exit" _ = return ()
processInput input myEnv = case parser input of
    Just sexpr -> do
        let ast = convertSExprToAst sexpr
        let (eval, newEnv) = evaluation ast myEnv
        print eval
        _ <- loopInput newEnv
        return ()
    Nothing -> do
        putStrLn "Invalid input"
        _ <- loopInput myEnv
        return ()

loopInput :: Env -> IO ()
loopInput myEnv = do
    result <- catch getLine handleEOF
    processInput result myEnv
    where
        handleEOF :: IOException -> IO String
        handleEOF _ = return "exit"

main :: IO ()
main = do
    let myEnv = createEnv
    loopInput myEnv
