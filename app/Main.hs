module Main (main) where

import Control.Exception (catch, IOException)
import AST.Constants()
import Parser.Parser
import AST.Tree.ConvertToAst
import VirtualMachine.Evaluation
import AST.Env
import Lib()

processInput :: String -> AST.Env.Env -> IO ()
processInput "exit" _ = return ()
processInput input myEnv = case parser input of
    Just sexpr -> do
        case convertAllChiasseToAst sexpr of
            Just asts -> do
                print asts
            Nothing -> do
                putStrLn "Invalid input"
                _ <- loopInput myEnv
                return ()
    Nothing -> do
        putStrLn "Invalid input"
        _ <- loopInput myEnv
        return ()

loopInput :: Env -> IO ()
loopInput myEnv = do
    result <- catch getContents handleEOF
    processInput result myEnv
    where
        handleEOF :: IOException -> IO String
        handleEOF _ = return "exit"
main :: IO ()
main = do
    let myEnv = createEnv
    loopInput myEnv
