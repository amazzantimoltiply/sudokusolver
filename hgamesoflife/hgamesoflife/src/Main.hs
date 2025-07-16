module Main (main) where

import Lib (glider)


main :: IO ()
main = do
  putStrLn "Welcome to the Game of Life!"
  putStrLn "Here is a glider pattern:"
  print glider
  putStrLn "You can implement further game logic here."
  -- Further game logic can be added here, such as running the simulation, displaying the board, etc.
