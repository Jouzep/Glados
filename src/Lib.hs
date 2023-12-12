module Lib
    ( someFunc,
    add
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- src/Lib.hs

add :: Int -> Int -> Int
add x y = x + y