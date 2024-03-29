module Main (main) where

import Lib

main :: IO ()
main = do 
    print $ myUnlines (mylines "Andrea\nCiao\nCome\nStai\n")
    


