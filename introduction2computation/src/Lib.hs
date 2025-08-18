module Lib
    ( primes,
      upto,
      countDoubled,
      reverseList,
      reverseListRecursive
    ) where
primes :: [Int]
primes = 2 : [n | n <- [3,5..], isPrime n]
  where
    isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p*p <= n) primes)
upto :: (Ord a) => a -> [a] -> [a]
upto n = takeWhile (<= n)

countDoubled::String->Int
countDoubled [] = 0
countDoubled xs = length [x | (x,y) <- zip xs (tail xs), x==y ]

reverseList :: [a] -> [a]
reverseList = foldl (flip (:)) []

-- The explicit recursive version for comparison
reverseListRecursive :: [a] -> [a]
reverseListRecursive xs = reverseAcc xs []
  where
    reverseAcc :: [a] -> [a] -> [a]
    reverseAcc [] acc = acc
    reverseAcc (x:xs) acc = reverseAcc xs (x:acc)