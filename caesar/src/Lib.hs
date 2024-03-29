{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Lib where

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

isLower::Char->Bool
isLower ch = elem ch lowerAlphabet

isUpper::Char->Bool
isUpper ch = elem ch upperAlphabet

isDigit::Char->Bool
isDigit ch = elem ch digits

digits :: Alphabet
digits = ['0' .. '9']

square :: Int -> Int
square x = x * x

listLenght::String->Int
listLenght []=0
listLenght (x:xs) = 1 + listLenght xs

indexOf :: (Eq a, Integral s) => a -> [a] -> s -> Maybe s
indexOf _ [] _ = Nothing
indexOf a (x:xs) s
    | a == x    = Just s
    | otherwise = indexOf a xs (s+1)


findChar::Char->String->Bool
findChar _ [] = False
findChar ch (x:xs) = (ch == x) || findChar ch xs

charByPos::Int->String->Char
charByPos _ [] = undefined
charByPos 0 (x:xs) = x
charByPos p (x:xs) = charByPos (p-1) xs

upperRot::Int -> Char -> Char
upperRot n ch = upperAlphabet !! mod
    (case indexOf ch upperAlphabet 0 of
        Just a -> a + n
        Nothing ->0)
    26


lowerRot::Int -> Char -> Char
lowerRot n ch = lowerAlphabet !! mod
    (case indexOf ch lowerAlphabet 0 of
        Just a -> a + n
        Nothing ->0)
    26

rotChar::Char->Int->Char
rotChar ch n
    | isUpper ch = upperRot n ch
    | isLower ch = lowerRot n ch
    | otherwise = ch

caesar::Int->String->String
caesar _ [] = []
caesar n message = map (\ch-> rotChar ch n) message

transform fun[]=[]
transform fun (x:xs) = fun x : transform fun xs

count::Char->String->Int
count _ [] = 0
count c (x:xs) = if c == x then 1 + count c xs else 0 + count c xs