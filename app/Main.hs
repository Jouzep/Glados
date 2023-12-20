module Main (main) where

import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Data.IORef
import Evaluation.Evaluation (evaluation)
import AST.Constants
import AST.Env

main :: IO ()
main = do
    let a = Var (AstInt 5)
    let e = Var (AstSymb "a")
    let operation = Define "a" (Var (AstInt 5))
    let myEnv = createEnv
    let (b, myEnv1) = evaluation operation myEnv
    putStrLn(show (b))
    printEnv myEnv1
    let operation1 = BinaryOp Add a e
    let (b1, myEnv2) = evaluation operation1 myEnv1
    putStrLn(show (b1))
    printEnv myEnv2
    case b1 of
        Just result -> putStrLn (show (Just result)) >>  exitSuccess
        Nothing -> putStrLn "Failed" >> exitWith (ExitFailure 84)
