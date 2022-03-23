module Lib
    ( someFunc, pares
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pares [] = []
pares (x:xs) = if even x then x: pares xs
                         else pares xs