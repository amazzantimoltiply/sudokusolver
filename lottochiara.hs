
import System.Random
import Data.List

genNRndNum::Int->[Int]->[Int]
genNRndNum 0 acc= acc
genNRndNum num acc =
        do g <- newStdGen
           let newval = head (randomRs (1,90) g :: [Int]) 
           genNRndNum (num -1) (newval:acc) 
main = do
    print (genNRndNum 6 [])
    --print . take 6 . nub $ (randomRs (1,90) g :: [Int])