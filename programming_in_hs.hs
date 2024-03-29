n = div a (length xs)
    where
        a = 10
        xs = [1,2]

halve::[Integer] -> ([Integer],[Integer])
halve xs = if not (null xs) then
        splitAt (div (length xs) 2) xs
        else ([],[])
{-
third::[Integer] -> Integer
third xs | length xs >= 3 = xs !! 2
         | otherwise = 0
-}

add::Integer -> Integer -> Integer
add = \x -> \y -> x + y

inc::Integer->Integer
inc = add 1

mul::Integer -> Integer -> Integer -> Integer
mul = \x -> \y -> (\z -> x*y*z)

{-
third::[Integer] -> Integer
third [_,_,x]=x
-}

third::[Integer] -> Integer
third xs =  head ( tail (tail xs))

safetail::[Integer]->[Integer]
safetail xs | null xs  = xs
            | otherwise = tail xs

(||)::Bool -> Bool -> Bool
True || True = True
_ || _ = False

luhnDouble::Integer -> Integer
luhnDouble x | (x * 2) > 9 = (x * 2) - 9
             | otherwise = x
factors::Int -> [Int]
factors n = [x | x <- [1..n], mod n x ==0]

prime::Int->Bool
prime n = factors n == [1,n]

primes::Int->[Int]
primes n = [x| x<-[2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

pairs::[a]->[(a,a)]
pairs xs = zip xs (tail xs)

sorted:: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

main=do
        print "printing prime 2"
        print (prime 2)
