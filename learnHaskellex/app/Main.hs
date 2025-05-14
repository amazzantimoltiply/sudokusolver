module Main (main) where

import Lib

main :: IO ()
main = do
    --putStrLn "Hello, Haskell!"
    --let list = [1::Int, 2, 3, 4, 5]
    --putStrLn $ "The list is: " ++ show list
    --let index = 2
    --let value = indexOfCharAt list index
    --putStrLn $ "The value at index " ++ show index ++ " is: " ++ show value
    let list = ['a', 'b', 'c', 'a', 'd', 'a']
    let char = 'a'
    let count = countNOfCharAt list char
    putStrLn $ "The count of " ++ show char ++ " in the list is: " ++ show count
    