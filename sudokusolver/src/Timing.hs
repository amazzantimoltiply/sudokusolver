module Timing (
    timeFunction,
    comparePerformance
) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Control.Exception (evaluate)
import Control.DeepSeq (NFData, force)

-- | Time a function and return its result along with the execution time in seconds
timeFunction :: NFData a => String -> (() -> a) -> IO (a, Double)
timeFunction name f = do
    start <- getCurrentTime
    result <- evaluate $ force $ f ()
    end <- getCurrentTime
    let diff = realToFrac $ diffUTCTime end start
    putStrLn $ name ++ " took " ++ show diff ++ " seconds"
    return (result, diff)

-- | Compare the performance of two functions
comparePerformance :: (NFData a, Eq a) => String -> String -> (() -> a) -> (() -> a) -> IO ()
comparePerformance name1 name2 f1 f2 = do
    putStrLn "Starting performance comparison..."
    (result1, time1) <- timeFunction name1 f1
    (result2, time2) <- timeFunction name2 f2
    putStrLn $ "\nResults " ++ if result1 == result2 then "match" else "differ!"
    putStrLn $ name1 ++ "/" ++ name2 ++ " ratio: " ++ show (time1 / time2)
    putStrLn $ "Absolute difference: " ++ show (abs (time1 - time2)) ++ " seconds"
