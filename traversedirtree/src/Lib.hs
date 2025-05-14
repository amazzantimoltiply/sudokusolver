module Lib
    (dropSuffix,classifyFile,FileType
    ) where
import Data.List (isSuffixOf)
import System.Directory ( canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory )

data FileType = FileTypeDir | FileTypeFile | FileTypeOther deriving Show

dropSuffix::String->String->String
dropSuffix suffix str
    | suffix `isSuffixOf` str = take ( length str - length suffix) str
    | otherwise = str

classifyFile::FilePath -> IO FileType
classifyFile fName = do
    isDir <- doesDirectoryExist fName
    isFile <- doesFileExist fName
    return $ case (isDir,isFile) of
        (True,False) -> FileTypeDir
        (False,True) -> FileTypeFile
        _Otherwise -> FileTypeOther