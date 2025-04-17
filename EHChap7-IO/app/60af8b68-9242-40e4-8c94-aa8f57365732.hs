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

ioShow::String->IO (IO String)
ioShow str = return (return (show str))

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
    print (ioShow "test" >>= \io1 io2 -> io2)


