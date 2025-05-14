module Lib
    ( indexOfCharAt,
      countNOfCharAt
    ) where

indexOfCharAt::[a] -> Int -> a
indexOfCharAt (a:as) 0 = a
indexOfCharAt (a:as) n = indexOfCharAt as (n-1)
indexOfChatAt [] _ = error "Index out of bounds"


countNOfCharAt::Eq a => [a] -> a -> Int
countNOfCharAt [] _ = 0
countNOfCharAt (a:as) n 
    | a == n = 1 + countNOfCharAt as n
    | otherwise = countNOfCharAt as n       