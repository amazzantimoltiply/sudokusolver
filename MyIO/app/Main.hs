module Main (main) where
import Data.Char
readInts::Int->IO [Int]
readInts 0 = return []
readInts n = do x <- readLn
                xs <- readInts (n-1)
                return (x:xs)

main :: IO ()
main = do
        putStrLn "Emter 3 numbers:"
        ints <- readInts 3
        putStrLn "Sum is:"
        print (sum ints)
