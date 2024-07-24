{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

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
                numstring = numToString!! index  ++ " " ++ result
                picknum = numToString !! n1
                index = mod n1 10


takeWords s = takeW' s []
    where
      takeW' s1 acc1
        | null s1 = acc1
        | otherwise = takeW' lefts nextaccn 
          where
              lefts = dropWhile (==' ') . dropWhile (/= ' ') $ s1
              nextaccn = acc1 ++ takeWhile (/= ' ') s1

main :: IO ()
main = do
           -- let usAM = RegisteredUser (Username "Andrea") (AccountN 1234)
           -- printUser usAM
           --employeeRank Coder CEO
           --print (dividedBy 10 3)
           --print (numToWord 12756)
           --print (divByTen 125)
           putStrLn (takeWords "ciao andrea come stai")
