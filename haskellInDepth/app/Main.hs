

module Main (main) where
import Lib ( extractVocab, printVocab 
            , allWords, wordsCount
            , wordsByFrequency, allWordsReport
            , wordsCountReport, frequentWordsReport)
import qualified Data.Text.IO as TIO
import Fmt ()




main :: IO ()

main = do
    text <- TIO.readFile "shake.txt"
    let vocab = extractVocab text
    printVocab vocab
   
 