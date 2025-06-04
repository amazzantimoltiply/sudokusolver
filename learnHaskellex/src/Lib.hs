module Lib
    ( indexOfCharAt,
      countNOfCharAt
    ) where

indexOfCharAt::[a] -> Int -> a
indexOfCharAt [] _ = error "Index out of bounds"
indexOfCharAt (a:_) 0 = a
indexOfCharAt (_:as) n = indexOfCharAt as (n-1)


countNOfCharAt::Eq a => [a] -> a -> Int
countNOfCharAt [] _ = 0
countNOfCharAt (a:as) n 
    | a == n = 1 + countNOfCharAt as n
    | otherwise = countNOfCharAt as n       