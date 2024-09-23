

import Test.Hspec
import Test.QuickCheck ()
import Control.Exception (evaluate)

subs::[a]->[[a]]
subs [] = [[]]
subs (x:xs) = subs xs ++ map (x:) (subs xs)

cp::[a]->[a]->[(a,a)]
cp xs ys = [(x,y)| x <-xs,y<-ys]

splits::[a]->[(a,[a])]
splits xs = [(xs!!k, take k xs ++ drop (k+1) xs) | k <- [0..length xs-1] ]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = [ y:zs | (y,ys) <- splits (x:xs), zs <- perms ys ]

partitions :: Int -> [[Int]]
partitions 0 = [[]]
partitions n | n > 0 = [ k : xs | k <- [1..n], xs <- partitions (n-k), all (k <=) xs ]


main :: IO ()
main = hspec $ do
    describe "test combinatorial functions" $ do
        it "subs must returns all the combinations of subs" $ do
            shouldBe (subs "abc") ["", "c", "b", "bc", "a", "ac", "ab", "abc"] 
    describe "test cartesian product" $ do
        it "should return all the possible combination of pair" $ do
            shouldBe (cp "abc" "bcd") [('a','b'),('a','c'),('a','d'),('b','b'),('b','c'),('b','d'),('c','b'),('c','c'),('c','d')]
    describe "test spitting a list" $ do
        it "should return all the possible split of a list" $ do
                    shouldBe (splits "abc") [('a',"bc"),('b',"ac"),('c',"ab")]
    describe "test permutations of a list" $ do
        it "should return all the possible permutations of a list usin splits" $ do
                    shouldBe (perms "abc") ["abc", "acb", "bac", "bca", "cab", "cba"]
    describe "test partitions of a number" $ do
        it "should return all the possibile partitions that added up to a number" $ do
                    shouldBe (partitions 2) [[1,1],[2]]