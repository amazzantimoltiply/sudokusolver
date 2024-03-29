module Main (main) where

import Lib
import System.Environment

main :: IO ()
main = do
        cliArgs <- getArgs
        let mFilePath = parseArguments cliArgs
        
        do 
            file <- case mFilePath of
                    Nothing -> return [""]
                    Just a -> readLines a
            print $ numberAllLines file 
            

