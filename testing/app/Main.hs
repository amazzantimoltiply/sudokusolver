module Main (main) where

--import Lib
import Test.QuickCheck
import Data.Char

test_reverse_prop::Eq a=>[a]->Bool
test_reverse_prop ns = reverse (reverse ns) == ns

test_sum_prop::Int->Int->Bool
test_sum_prop x y = (x + y >= x) && (x + y >= y)

capitalize::String->String
capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

test_is_capitalized_prop::String->Bool
test_is_capitalized_prop "" = False
test_is_capitalized_prop fs = check (capitalize fs) where
    check (c:cs) = isUpper c &&
                   length fs == length (c:cs)

myinsert::Ord a=>a->[a]->[a]
myinsert n [] = [n]
myinsert n (n':ns)
    | n < n' = n:n':ns
    | otherwise = n' : myinsert n ns

merge::Ord a=>[a]->[a]->[a]
merge [] ds = ds
merge cs [] = cs
merge (c:cs) (d:ds)
    | c > d = d : merge (c:cs) ds
    | otherwise = c : merge cs (d:ds)

myEnumFromTo::(Ord a,Num a) =>a->a->[a]
myEnumFromTo start end
    | end < start = []
    | otherwise = start : myEnumFromTo (start+1) (end-1)

allPosAndDivby3::(Ord a,Num a)=>[a]->Bool
allPosAndDivby3 = all (>0)

myinsertionSort::Ord a=>[a]->[a]
myinsertionSort = foldr myinsert []

test_insert_one_elem_prop::Ord a=>a->[a]->Bool
test_insert_one_elem_prop c cs = check (myinsert c cs) where
                                    check (c:c':_) = c <= c'

halvesEvenRecs::(Ord a,Integral a)=>[a]->[a]
halvesEvenRecs [] =[]
halvesEvenRecs (c:cs)
        | even c = c : halvesEvenRecs cs
        | otherwise = halvesEvenRecs cs


main :: IO ()
main = do
    --quickCheck (test_reverse_prop "and")
    --quickCheck (test_sum_prop 1 1)
    --quickCheck (test_is_capitalized_prop "andrea")
    --quickCheck (test_insert_one_elem_prop 2 [5,3])
    --print (myinsertionSort [3,7,6,5,4,3,2])
    --print (halvesEvenRecs [1,2,20,27,55,70,88])
    --print (merge [1,3] [2,4])
    --print (myEnumFromTo 0 6)
    print (allPosAndDivby3 [3,6,9,12])
