{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main (main) where
import GHC.IO
import GHC.IO.IOMode
import GHC.IO.Handle (hGetContents)
import GHC.IO.Handle (hClose)
import GHC.IO.StdHandles (openFile)
import System.Environment (getArgs)
import Text.Read (readMaybe)

makeAndReadFiles::Int->IO String
makeAndReadFiles fnumber =
        let fname = show fnumber
        in writeFile fname fname >> readFile fname

makeAndShowFiles::Int -> IO ()
makeAndShowFiles n =
    makeAndReadFiles n >>= putStrLn

unsafe::IO()
unsafe =
    let files = mapM makeAndReadFiles [1..50000]
    in files >>= (print . show)

safe :: IO ()
safe =
    mapM_ makeAndShowFiles [1..50]

ioRead::String->IO Int
ioRead numString = return (read numString)

myIoAction::(Num a)=>a->IO (IO a)
myIoAction val = return (return val)





listOfActions::[a]->[IO a]
listOfActions = map return


showIoAction ioa =
    ioa >>= \io -> io >>= \str -> putStrLn str
doSomeFileStuff =
    openFile "/haskellprj/EHChap7-IO/test.txt" ReadMode
        >>= \handle -> hGetContents handle
        >>= \contents -> putStrLn contents
        >>= \_ -> hClose handle

sumArgs::[String]->Maybe Int
sumArgs strArgs =
    let intArgs = mapM readMaybe strArgs
    in fmap sum intArgs

main :: IO ()
main = do
    --doSomeFileStuff
    --getArgs >>= print . sumArgs
    --safe
    --showIoAction (myIoAction 123)
    print "Done"
