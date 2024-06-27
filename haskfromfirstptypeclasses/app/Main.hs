{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Main (main) where

import Lib

data Trivial = Trivial
instance Eq Trivial where
    Trivial == Trivial = True

data TisAnInteger = MkAnInt Int
instance Eq TisAnInteger where
    (==) (MkAnInt a) (MkAnInt a1) = a == a1

data TwoInts = MkTwoInts Int Int
instance Eq TwoInts where
    (==) (MkTwoInts a a1) (MkTwoInts b b1) = (a == b) && (a1 == b1)

data StringOrInts = TisAnInt Int | TisAString String
instance Eq StringOrInts where
    (==) (TisAnInt a) (TisAnInt a1) = a==a1
    (==) (TisAString s) (TisAString s1) = s==s1
    (==) (TisAnInt a) (TisAString s) = False
 
data Pair a = Pair a a 

instance Eq a => Eq (Pair a)  where
    (==) (Pair a1 a2) (Pair a3 a4 ) = a1 == a3 && a2 == a4

main :: IO ()
main = do
    --print (Trivial == Trivial)

    print (MkAnInt 2 == MkAnInt 1)
    print (MkTwoInts 2 2 == MkTwoInts 2 2)
    print (TisAnInt 2 == TisAString "Ciao")
    print (TisAString "Ciao" == TisAString "Ciao")
    print (Pair 1 2 == Pair 1 2)
