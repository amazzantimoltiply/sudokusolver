{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Lib

import Data.IORef ( newIORef, readIORef, writeIORef )

readWriteIORef::IO Int
readWriteIORef = do
    myRef <- newIORef @Int 0
    writeIORef myRef 7
    readIORef myRef

main :: IO ()
main = do
    readWriteIORef >>= print
