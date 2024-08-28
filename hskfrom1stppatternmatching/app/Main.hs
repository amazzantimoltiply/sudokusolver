{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Data.Char
data Employee = Coder | Manager | Veep | CEO deriving (Eq,Ord,Show)

data WherePenguinsLive =
    Galapagos
    | Antarctica
    | Australia
    | SouthAfrica
    | SouthAmerica
        deriving (Eq, Show)

newtype Penguin = Peng WherePenguinsLive deriving (Eq, Show)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a,b,c) (d,e,f) = ((a,d), (c,f))


reportBoss::Employee->Employee->IO()
reportBoss e e' = putStrLn $ show e ++ " is boss of " ++ show e'

employeeRank:: Employee -> Employee -> IO()
employeeRank e e'
    | e > e' = reportBoss e e'
    | e == e' = putStrLn "Neither employee is the boss"
    | e < e' = reportBoss e' e
    | otherwise = putStrLn "Neither employee is the boss"

newtype Username = Username String
newtype AccountN = AccountN Integer

data User = UnRegisteredUser | RegisteredUser Username AccountN

printUser::User -> IO()
printUser UnRegisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser
                (Username name)
                (AccountN accn)) = putStrLn $ name ++ " " ++ show accn

dividedBy::Integral a =>a->a->(a,a)
dividedBy n d = dividedBy' n d 0 where
    dividedBy' n d count
       | n < d = (count,n)
       | otherwise = dividedBy' (n -d) d (count+1)

numToString::[String]
numToString = ["zero","uno","due","tre","quattro","cinque","sei","sette","otto","nove"]

divByTen::Int->Int
divByTen num = divBT' num 0
    where
        divBT' val acc
            | val < 10 = acc
            | otherwise = divBT' (div val 10) (acc + mod val 10)


numToWord::Int->[Char]
numToWord n = numToWord' n "" where
    numToWord' n1 result
       | n1 < 10 =  picknum ++ " " ++ result
       | otherwise = numToWord' val numstring
            where
                val = div n1 10
                numstring = numToString !! index  ++ " " ++ result
                picknum = numToString !! n1
                index = mod n1 10


takeWords s = if null s then [] else
    takeW' s []
    where
      takeW' s1 acc1
        | null s1 = acc1
        | otherwise = takeW' nextword rest
          where
              isSpace = flip elem " "
              trim = dropWhile isSpace
              nextword = trim . dropWhile (not . isSpace ) $ s1
              rest = acc1 ++ takeWhile (/= ' ') s1

myzip::[a]->[b]->[(a,b)] 
myzip [] _ = []
myzip _ [] = []
myzip (a:as) (b:bs)= (a,b) : myzip as bs

myZipWith::(a->b->c)->[a]->[b]->[c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

filterAllUp::String->String
filterAllUp = filter isUpper

capTheFirst::String->String
capTheFirst (x:xs) = toUpper x : xs

mysquare = [x^2|x<-[1..5]]
myqube = [y^3|y<-[2..7]]

mymix = [(x, y) | x <- mysquare, x < 50, y <- myqube, y < 50]

myElem::Eq a=>a->[a]->Bool
myElem _ [] = False
myElem v (x:xs) = (v==x) || myElem v xs

myremovew::String->[String]
myremovew = filter (\x -> notElem x ["the","a","an"]) . words

myReverse::[a]->[a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

main :: IO ()
main = do
           -- let usAM = RegisteredUser (Username "Andrea") (AccountN 1234)
           -- printUser usAM
           --employeeRank Coder CEO
           --print (dividedBy 10 3)
           --print (numToWord 12756)
           --print (divByTen 125)
           --putStrLn (takeWords "ciao andrea come stai")
           --print mymix
           --print (myremovew "the rocket is flying to the moon")
           --print (myzip [1,2][3,4])
           --print (myZipWith (+) [1,2] [3,4])
           --print (myZipWith (,) [1,2] [3,4])
           print (filterAllUp "HsdfEdmfLwqxLcdrtO")
           print (capTheFirst "andrea")
           print (myElem 5 [1,2,3])
           print (myReverse "andrea")
