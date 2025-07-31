module Lib
    ( primes,
      upto
    ) where
primes :: [Int]
primes = 2 : [n | n <- [3,5..], isPrime n]
  where
    isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p*p <= n) primes)
upto :: (Ord a) => a -> [a] -> [a]
upto n = takeWhile (<= n)