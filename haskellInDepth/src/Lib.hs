{-# LANGUAGE OverloadedStrings #-} 

module Lib
    ( extractVocab,
      Entry,
      Vocabulary,
      printVocab,
      allWords,
      wordsCount,
      wordsByFrequency,
      allWordsReport,
      wordsCountReport,
      frequentWordsReport
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt

import Data.Char
import Data.List (group,sortOn,sort)
import Data.Ord


type Entry=(T.Text,Int)
type Vocabulary=[Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab t = map buildEntry $ group $ sort ws
 where
    ws = map T.toCaseFold $ filter (not . T.null) $ map cleanWord $ T.words t
    buildEntry xs@(x:_) = (x, length xs)
    cleanWord = T.dropAround (not . isLetter)

printVocab :: Vocabulary -> IO ()
printVocab vocab = do
    print $ T.unwords $ map fst vocab
    print $ length vocab

                                    
allWords::Vocabulary-> [T.Text]
allWords = map fst

wordsCount::Vocabulary-> (Int,Int)
wordsCount vocab = (length vocab, sum $ map snd vocab)

wordsByFrequency::Vocabulary-> Vocabulary
wordsByFrequency = sortOn (Down . snd) 

allWordsReport :: Vocabulary -> T.Text
allWordsReport vocab =
 fmt $ nameF "All words" $ unlinesF (allWords vocab)

wordsCountReport :: Vocabulary -> T.Text
wordsCountReport vocab = fmt $
    "Total number of words: " +|total|+
    "\nNumber of unique words: " +|unique|+ "\n"
 where
   (total, unique) = wordsCount vocab

frequentWordsReport :: Vocabulary -> Int -> T.Text
frequentWordsReport vocab num =
   fmt $ nameF "Frequent words"
       $ blockListF' "" fmtEntry reportData
 where
   reportData = take num $ wordsByFrequency vocab
   fmtEntry (t, n) = ""+|t|+": "+|n|+""