
module Lib
    (
    interactiveLines,
    printHelpText,
    parseArguments,
    readLines,
    numberAllLines,
    isEmpty
    ) where
import Data.Char
import System.Environment

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]


interactiveLines::Int ->IO()
interactiveLines counter = do
    line <- getLine
    if null line then
        return ()
    else do
        putStrLn (show counter ++ " . " ++ map toUpper line)
        interactiveLines (counter +1)

printHelpText :: String -> IO ()
printHelpText msg = do
        putStrLn (msg ++ "\n")
        progName <- getProgName 
        putStrLn ("Usage: " ++ progName ++ " <filename>") 
        
parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath 
parseArguments _ = Nothing 

readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath 
  return (lines contents)

numberAllLines :: (String -> Bool) -> (String -> Bool) -> [String] ->NumberedLines
numberAllLines shouldIncr shouldNumber lines =
      let go :: Int -> [String] -> NumberedLines
          go _ [] = []
          go counter (x : xs) =
            let mNumbering = if shouldNumber x then Just counter else Nothing
                newCounter = if shouldIncr x then counter + 1 else counter
             in (mNumbering, x) : go newCounter xs
       in go 1 lines

isEmpty :: String -> Bool
isEmpty str =
       null str
         || all (\s -> not (isPrint s) || isSeparator s) str