module Main (main) where
import Lib
import Parser.Parser

main :: IO ()
main =
    case parser "(define add (lambda (x y) (+ x y)))" of
        Just result -> print result
        Nothing -> putStrLn "Nothing"
