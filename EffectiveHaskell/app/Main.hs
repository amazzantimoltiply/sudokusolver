module Main (main) where

import Lib

data List a = Nil | Cons a (List a)
lenghList::List a -> Int
lenghList Nil = 0
lenghList (Cons x xs) = 1 + lenghList xs
verifyNumber num
  | num > 0 = let positive = "Positive" in exclaim positive
  | num < 0 = let negative = "Negative" in exclaim negative
  | num < 3 = exclaim "Small"
  | num < 100 = let medium = "Medium" in exclaim medium
  | otherwise = exclaim "Large"
  where
    exclaim str = show num <>" is " <> str <> " "

myZipWith::(a->b->c)->[a]->[b]->[c]
myZipWith fun as bs = [fun a b | a<- as,b <-bs]

--myZipWithRec::(a->b->c)->[a]->[c]->[c]
--myZipWithRec _ [] (b:bs) = [b]
--myZipWithRec _ (a:as) [] = [a]
--myZipWithRec f (a:as) (b:bs) = f a b : myZipWithRec f as bs

--myZipWithFold::(a->a->a)->[a]->[a]->[a]
--myZipWithFold f aas bbs = foldr f . streamLists aas bbs  where
--  streamLists [] (b:bs) = b
--  streamLists (a:as) [] = a
--  streamLists (a:as) (b:bs) = a : b : streamLists as bs


sumOfDiff xxs = sumBiggest xxs - sumSmallest xxs where
                    sumBiggest xxs = sum [maximum xs | xs <-xxs]
                    sumSmallest xxs = sum [minimum xs | xs <-xxs]



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
    -- print (tails' [1,2,3,4,5])
    -- print (verifyNumber (-3))
    -- print (myZipWith (,) [1,2] [1,2])
    -- print (myZipWithRec (,) $ [1,2] [1,2])
    --print (myZipWithFold (+) [1,2,3] [1,2,3])
    --print (sumBiggest [[1,2,3],[1,2,3]])
    --print (sumBiggest [[1,2,3],[1,2,3]])
    print (sumOfDiff [[1,2,3],[1,2,3]])