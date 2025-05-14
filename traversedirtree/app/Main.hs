{-# LANGUAGE TypeApplications #-}
	module Main where

import Control.Exception (IOException, handle)  
import Control.Monad (join,void,when)  
import Data.Foldable (for_)  
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isSuffixOf) 
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory )   
import qualified Data.Set as Set (empty, insert, member)   
import Text.Printf (printf)
import Lib (dropSuffix,classifyFile,FileType)




main :: IO ()
main = do
    --print (dropSuffix "\\" "dir1\\")
    classifyFile "C:\\haskellprj\\traversedirtree" >>= print