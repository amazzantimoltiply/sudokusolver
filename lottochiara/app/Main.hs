import System.Random
import System.Random.Shuffle (shuffle')  -- We use shuffle' instead of shuffleM
import Control.Monad (liftM)

-- Function to generate unique random numbers
getnRNumbers :: Int -> IO [[Int]]
getnRNumbers howmany = do
    seed <- generateSeed  -- Get an initial random seed
    generateUniqueNumbers howmany seed  -- Generate unique random numbers using the initial seed

-- Recursive function to generate unique random number lists
generateUniqueNumbers :: Int -> StdGen -> IO [[Int]]
generateUniqueNumbers 0 _ = return []  -- Base case: when we need 0 numbers, return an empty list
generateUniqueNumbers n gen = do
    let (randomNumbers, gen') = generateUniqueRandomNumbers 6 1 90 gen  -- Generate 6 unique random numbers
    restOfNumbers <- generateUniqueNumbers (n - 1) gen'  -- Recursively generate the remaining lists with the updated generator
    return (randomNumbers : restOfNumbers)  -- Combine the current list with the rest

-- Function to generate a random seed (StdGen type)
generateSeed :: IO StdGen
generateSeed = do
    seed <- randomIO  -- Generate a random seed using randomIO
    return (mkStdGen seed)  -- Convert the seed to a random number generator (StdGen)

-- Function to generate a list of unique random numbers given a seed
generateUniqueRandomNumbers :: Int -> Int -> Int -> StdGen -> ([Int], StdGen)
generateUniqueRandomNumbers count min max gen = 
    (uniqueNumbers, newGen)  -- Return the unique numbers and the updated generator state
  where
    allNumbers = [min..max]  -- List of all possible numbers
    shuffledNumbers = shuffle' allNumbers (length allNumbers) gen  -- Shuffle the numbers using shuffle'
    uniqueNumbers = take count shuffledNumbers  -- Take the first 'count' unique numbers from the shuffled list
    newGen = snd (next gen)  -- Get the new generator state after generating the numbers


main :: IO ()
main = do
         randomLists <- getnRNumbers 1  -- Generate 3 lists of random numbers
         print randomLists
         randomLists1 <- getnRNumbers 1  -- Generate 3 lists of random numbers
         print randomLists1
         randomLists2 <- getnRNumbers 1  -- Generate 3 lists of random numbers
         print randomLists2
         randomLists3 <- getnRNumbers 1  -- Generate 3 lists of random numbers
         print randomLists3