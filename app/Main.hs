module Main (main) where
import Lib

import System.Exit (exitSuccess, exitWith, ExitCode(..))

import Evaluation.Evaluation (evaluation)
import AST.Constants

main :: IO ()
main = do
    let a = Var (AstInt 5)
    let d = BinaryOp Add (BinaryOp Multiply (BinaryOp Sub (Var (AstInt 10)) (Var (AstInt 5))) (BinaryOp Divide (Var (AstInt 20)) (Var (AstInt 4)))) (BinaryOp Add (Var (AstInt 15)) (Var (AstInt 3))) -- 1
    let s = BinaryOp GreaterThan d a -- should return true
    -- let c = BinaryOp (AstBinaryOp Add (Var (AstInt 5)) (Var (AstInt 5)))
    let cond = Cond s a d -- if s true then a else d
    -- let c = BinaryOp Add (Var (AstInt 5)) (BinaryOp Add (Var (AstInt 5)) (Var (AstInt 5)))
    let b = evaluation(d)
    case b of
        Just b -> putStrLn (show b) >>  exitSuccess
        Nothing -> putStrLn "Failed" >> exitWith (ExitFailure 84)
