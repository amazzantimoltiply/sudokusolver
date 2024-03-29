module Main (main) where

import Lib

data List a = Nil | Cons a (List a)
lenghList::List a -> Int
lenghList Nil = 0
lenghList (Cons x xs) = 1 + lenghList xs

main :: IO()
main =
    --print ((addone . power2 . doubleinc ) 2) 
    --print (greatings "Chiara" "Fognani")
    --print (2 + 1 +++ 2 * 4)
    --print (makeGreet "Chiara" "Fognani")
    -- printSmallNum 9 
    -- print (fizzBuzz 15 1 "")
    -- print (fact 10)
    -- print (fibonacci 10)
    -- print (lenghList (Cons 1 (Cons 2 Nil)))
    print (tails' [1,2,3,4,5])

    

