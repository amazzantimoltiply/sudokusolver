module Main (main) where

import Lib (cp, cb)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)
import Control.Monad (replicateM)

timeFunction :: String -> ([[String]] -> [[String]]) -> [[String]] -> Int -> IO Double
timeFunction name f input iterations = do
    times <- replicateM iterations $ do
        start <- getCurrentTime
        result <- evaluate $ f input
        end <- getCurrentTime
        let diff = realToFrac $ diffUTCTime end start
        putStr "."
        return diff
    let avgTime = sum times / fromIntegral iterations
    putStrLn $ "\n" ++ name ++ " average time: " ++ show avgTime ++ " seconds"
    return avgTime

main :: IO ()
main = do
    putStrLn "Performance Comparison"
    putStrLn "---------------------"
    let input = replicate 20 (replicate 20 "1")
        iterations = 10000
    
    time1 <- timeFunction "cp" cp input iterations
    time2 <- timeFunction "cb" cb input iterations
    
    putStrLn $ "\nRatio (cp/cb): " ++ show (time1 / time2)
    putStrLn $ "Difference: " ++ show (abs (time1 - time2)) ++ " seconds"
