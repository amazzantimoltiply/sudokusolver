module Main (main) where

import Lib

units::[[Char]]
units = ["I","II","III","IV","V","VI","VII","VII","IX","X","XX","XXX","XL","L","LX","LXX","LXXX","XC","C","CC","CCC","CD","D","DC","DCC","DCCC","CM","M","MM","MMM"]

convertInttoRoman::[Int]->[[Char]]
convertInttoRoman xs =convertInttoRoman' xs units 0
convertInttoRoman'::[Int]->[[Char]]->Int->[[Char]]
convertInttoRoman' [] _ _= []
convertInttoRoman' (x:xs) rs u= rs!!(x+u-1) : convertInttoRoman' xs rs (u+9)


myreverse::[int]->[int]
myreverse = foldl (flip (:) ) []

mymap::(a->b)->[a]->[b]
mymap f = foldr op [] where op x xs = f x:xs

myfilter::(a->Bool)->[a]->[a]
myfilter p = foldr op [] 
   where 
     op x xs
       |p x = x:xs
       |otherwise = xs

integer = foldl shiftl 0 where shiftl n d = 10 * n + d 


main = do
    --print (reverse [1,2,3,4])
    print (mymap (1+) [1,2,3,4])
    print (myfilter even [1,2,4,5])
    print (integer[1,2,3])

