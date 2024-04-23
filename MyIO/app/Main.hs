module Main (main) where
import Data.Char
import Control.Monad
import System.IO

readInts::Int->IO [Int]
readInts 0 = return []
readInts n = do x <- readLn
                xs <- readInts (n-1)
                return (x:xs)
hReadLn :: Read a => Handle -> IO a
hReadLn h = do l <- hGetLine h
               return (read l)

main :: IO ()
main = do
        xs <- withFile "input.txt" ReadMode (\hIn ->
                do n <- hReadLn hIn
                   replicateM n (hReadLn hIn)
                   )
        print (xs::[Int])
    {- contents <- readFile "input.txt"
    let (l:ls) = lines contents
    let n = read l::Int
    let xs = map read (take n ls)
    writeFile "output.txt" (show (sum xs::Int))
    -}
    {-
        putStrLn "Emter 3 numbers:"
        ints <- readInts 3
        putStrLn "Sum is:"
        print (sum ints)
    -}