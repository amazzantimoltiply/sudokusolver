module Lib
    ( dropWhiteSpaces,dropWhiteSpaces',insertGen,iSortGen,summWithFold,tails',myScanR,average,deleteSame
    ) where

dropWhiteSpaces::String->String
dropWhiteSpaces s = go s [] where
    go::String->String->String
    go [] acc = acc
    go (c:cs) acc
        | (== ' ') c = go cs acc
        | otherwise = go cs (acc ++ [c])

dropWhiteSpaces'::String->String
dropWhiteSpaces' s = [c|c<-s,c/=' ']

insertGen :: (Int->Int->Bool) -> [Int] -> Int -> [Int]
insertGen _ [] y = [y]
insertGen f (x:xs) y
    |f y x     = x : insertGen f xs y
    |otherwise = y : x : xs

iSortGen::(Int->Int->Bool)->[Int]->[Int]
iSortGen f l = insertAll l [] where
    insertAll [] sl = sl
    insertAll (x:xs) sl = insertAll xs (insertGen f sl x)

summWithFold::[Int]->Int
summWithFold l = foldl u 0 l where
    u acc x = acc + x

tails'::[a]->[[a]]
tails' [] = []
tails' (_:xs) = xs : tails' (xs)


myScanR c acc l = map f (tails' l) where
    f l = scanr c acc l

average::[Float]->Float
average l = s / fromIntegral len where
    (s,len)= foldr c n l
    n=(0,0)
    c x (sr,lenr) = (sr+x, lenr +1)

deleteSame::Eq a =>[a]->a->[a]
deleteSame xs x = filter (/= x) xs 