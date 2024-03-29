module Main (main) where

import Control.Exception (catch, IOException)
import AST.Constants
import Parser.Parser
import AST.Tree.ConvertToAst
import VirtualMachine.Evaluation
import AST.Env
import AST.Compiler
import Lib()
import VirtualMachine.StackMachine

processInput :: String -> AST.Env.Env -> IO ()
processInput "exit" _ = return ()
processInput input myEnv = case parser input of
    Just sexpr -> do
        case convertAllChiasseToAst sexpr of
            Just asts -> do
                case compiler asts [] [] [] of
                    Just (insts, env) -> do
                        case exec env [] insts [] of
                            Right (returnValue) -> do
                                printChiasseResult returnValue
                                _ <- loopInput myEnv
                                return ()
                            Left (error) -> do
                                print error
                                _ <- loopInput myEnv
                                return ()
                    Nothing -> do
                        putStrLn "Invalid input compilerAllChiasse"
                        _ <- loopInput myEnv
                        return ()
            Nothing -> do
                putStrLn "Invalid input convertAllChiasseToAst"
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
