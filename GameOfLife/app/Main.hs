module Main where

import GameOfLife

main :: IO ()
main = do
    let grid = createGrid 10 10
    putStrLn "Welcome to Conway's Game of Life!"
    putStrLn "Starting with an empty 10x10 grid..."
