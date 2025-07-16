module Lib
    ( Vector3D(..)
    , MyEither(..),
        safeDivide
    , Card(..)
    , CardValue(..)
    , CardSuite(..)
    , Deck
    , fullDeck
    , smallDeck
    , Countable(..),
      Person(..),
      tell,
      loadFromFile
      , quicksort
    , trim'
    , groupPairs,
      convertDigitToSum
    ) where
import Data.Bifunctor
import Control.Applicative
import System.IO
import Data.Char (isSpace, digitToInt)

data Vector3D a = Vector3D
  { x :: a
  , y :: a
  , z :: a
  } deriving (Show, Eq)


instance Functor Vector3D where
  fmap f (Vector3D x y z) = Vector3D (f x) (f y) (f z)

data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq)
instance Functor (MyEither a) where
   fmap _ (MyLeft x) = MyLeft x
   fmap f (MyRight y) = MyRight (f y)

safeDivide :: (Fractional a, Eq a) => a -> a -> MyEither String a
safeDivide _ 0 = MyLeft "Division by zero"
safeDivide x y = MyRight (x / y)

instance Bifunctor MyEither where
  bimap f _ (MyLeft x) = MyLeft (f x)
  bimap _ g (MyRight y) = MyRight (g y)

data CardSuite = Club | Diamond | Heart | Spade deriving (Enum)

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine
  | Ten | Jack | Queen | King | Ace deriving (Enum)

data Card = Card CardValue CardSuite

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

type Deck = [Card]

instance Show CardValue where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"
instance Show CardSuite where
  show c = ["♣", "♦", "♥", "♠"] !! fromEnum c

instance Show Card where
  show (Card v s) = show v ++ " of " ++ show s

fullDeck :: Deck
fullDeck = [Card v s | v <- [Two .. Ace], s <- [Club .. Spade]]

smallDeck :: Deck
smallDeck = [Card v s | v <- [Two .. Five], s <- [Club .. Spade]]

data Countable a = Countable
  { count :: Int
  , value :: a
  } deriving (Eq)

instance Functor Countable where
  fmap f (Countable c v) = Countable (c+1) (f v)
  (<$) x (Countable c _) = Countable c x

instance Applicative Countable where
  (Countable c1 f) <*> (Countable c2 v) = Countable (c1 + c2) (f v)
  liftA2 f (Countable c1 v1) (Countable c2 v2) = Countable (c1 + c2) (f v1 v2)
  pure = Countable 0

instance Show a => Show (Countable a) where
  show (Countable c v) = "Count: " ++ show c ++ ", Value: " ++ show v

instance Monad Countable where
  return = pure
  (Countable c1 x) >>= f = let (Countable c2 y) = f x
                            in Countable (c1 + c2) y
tell :: String -> String
tell [] = "The list is empty"
tell [x] = "The list has only one element"
tell message = message
-- This function simulates sending a message to an agent and receiving a response.

-- Helper: group a list into pairs (tuples)
groupPairs :: [a] -> [(a, a)]
groupPairs (x:y:rest) = (x, y) : groupPairs rest
groupPairs _ = []



-- Helper: optimized trim whitespace (removes all leading/trailing isSpace, including newlines)
trim' :: String -> String
trim' = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

loadFromFile :: FilePath -> IO [Person]
loadFromFile filePath = do
  content <- readFile filePath
  let ls = lines content
      pairs = groupPairs ls
      people = [ Person (trim' name) age
               | (name, ageStr) <- pairs
               , not (null (trim' name))
               , [(age, "")] <- [reads ageStr :: [(Int, String)]]
               ]
  return people
  
-- | Implementation of the Quicksort algorithm.
-- This function sorts a list of ordered elements using the Quicksort algorithm.
-- The first element is used as a pivot, and the list is partitioned into
-- elements less than or equal to the pivot and elements greater than the pivot.
-- The algorithm recursively sorts these partitions.
--
-- >>> quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5]
-- [1,1,2,3,4,5,5,6,9]
--
-- Properties:
--   * Time complexity: O(n^2) worst case, O(n log n) average case
--   * Space complexity: O(n)
--   * In-place: No
--   * Stable: No
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p:xs) =
        let sortleft = filter (<= p) xs
            sortright = filter (> p) xs
        in quicksort sortleft ++ [p] ++ quicksort sortright
      
convertDigitToSum :: Int -> Int
convertDigitToSum = sum . map digitToInt . show . abs
  