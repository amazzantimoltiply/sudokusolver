module Main (main) where

import Lib
import System.Random
import Data.Time

guess::Int->IO()
guess n = do m <- readLn
             case compare n m of
               LT -> do putStrLn "Guess again, but lower: "
                        guess n
               GT -> do putStrLn "Guess again, but higher: "
                        guess n
               EQ -> return ()

main :: IO ()
main = do n <- randomRIO (1,100)
          putStrLn "Guess my number:"
          start <- getCurrentTime
          guess n
          end <- getCurrentTime
          putStrLn "Congratulations, you have guessed my number."
          putStrLn ("It took you:" ++ show (diffUTCTime end start))
