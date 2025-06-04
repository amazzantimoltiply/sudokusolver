{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Fmt
import qualified Data.Text as T

main :: IO ()
main = do
    fmtLn "Hello, world!"
    let xs = ["1", "2", "3"] :: [T.Text]
    fmtLn $ "Regular list: " +| xs |+ ""    
       

    --fmtLn $ "Using listF: " +| listF (xs :: [T.Text]) |+ "\n"
