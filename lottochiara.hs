import System.Random
import Data.List
main = do
    g <- newStdGen
    print . take 6 . nub $ (randomRs (1,90) g :: [Int])