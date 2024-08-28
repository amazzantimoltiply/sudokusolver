module Main (main) where

import Data.Time
import Data.Char
data DatabaseItem = DbString String
            | DbNumber Integer
            | DbDate UTCTime
                deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
        [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
                , DbNumber 9001
                , DbString "Hello, world!"
                , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
        ]

dbFilterDbDate::[DatabaseItem]->[UTCTime]
dbFilterDbDate = foldr listOfDates []where
    listOfDates (DbDate t) ts = t:ts
    listOfDates _ ts = ts

dbFilterDbNums::[DatabaseItem]->[Integer]
dbFilterDbNums = foldr listOfNums [] where
    listOfNums (DbNumber n) nms = n:nms
    listOfNums _ nms = nms


waterVolume::[Int]->Int
waterVolume h =
    sum (zipWith (-) (zipWith min (scanl max 1 h) (scanr max 1 h)) h)

stops = "pbtdkg"
vowels = "aeiou"


mulTheDigit vet = [digitToInt x|x <- vet, isDigit x]

myany f = foldr ((||) . f) False
myElem el = foldr ((||).(== el)) False

mixVowStop' = [(s,v,s1)| s<-stops,v<-vowels,s1<-stops]
main :: IO ()
main = do
    --print (dbFilterDbDate theDatabase)
    --print (dbFilterDbNums theDatabase)
    --print mixVowStop'
    --print (waterVolume [2,5,1,2,3,4,7,7,6])
    --print (myany even [1,3,5])
    --print (myany odd [1,3,5])
    --print (myElem 10 [0,0,1,0])
    print  (product (mulTheDigit "Il treno arriverà alle 16:35"))