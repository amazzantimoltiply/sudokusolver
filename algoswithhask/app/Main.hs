{-# LANGUAGE NumDecimals #-}
module Main (main) where

import Lib

perms::[a]->[[a]]
perms [] = [[]]
perms (x:xs) = [zs|ys<-perms xs,zs <- inserts x ys]

inserts::a->[a]->[[a]]
inserts x []=[[x]]
inserts x (y:ys) = (x:y:ys):map (y:) (inserts x ys)

binarySearch::(Ord a,Integral a)=>[a]->a->Bool
binarySearch xs sv = binarySearch' xs sv 0 (length xs - 1)
    where
        binarySearch'::(Ord a,Integral a)=>[a]->a->a->a->Bool
        binarySearch' xs sv l h
            | l <= h = False
            | guess == sv = True
            | guess < sv = binarySearch' xs sv l (mid-1)
            | guess > sv = binarySearch' xs sv (mid-1) h
            where
                mid = h-l
                guess = xs !! mid / 2
              
                


main :: IO ()
main = do
    print (perms [2,3,4,5])

