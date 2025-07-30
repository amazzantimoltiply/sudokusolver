module Lib
    ( Vector3D(..)
    , MyEither(..)
    , safeDivide
    , Card(..)
    , CardValue(..)
    , CardSuite(..)
    , Deck
    , fullDeck
    , smallDeck
    , Countable(..)
    , Person(..)
    , tell
    , loadFromFile
    , quicksort
    , trim'
    , groupPairs
    , convertDigitToSum
    , findTo
    , findKey
    , List(..)
    , toList
    , fromList
    , Tree(..)
    , singletonTree
    , insertTree
    , treeElem
    , height
    , balanceFactor
    , updateHeight
    ) where
import Data.Bifunctor
import Data.Char (isSpace, digitToInt)
import Data.List (find)

infixr 5 :-:
data List a = EmptyList | a :-: List a deriving (Show, Eq)

toList :: List a -> [a]
toList EmptyList = []
toList (x :-: xs) = x : toList xs

fromList :: [a] -> List a
fromList [] = EmptyList
fromList (x:xs) = x :-: fromList xs

data Vector3D a = Vector3D
  { x :: a
  , y :: a
  , z :: a
  } deriving (Show, Eq)

instance Num a => Num (Vector3D a) where
  (Vector3D x1 y1 z1) + (Vector3D x2 y2 z2) = Vector3D (x1 + x2) (y1 + y2) (z1 + z2)
  (Vector3D x1 y1 z1) * (Vector3D x2 y2 z2) = Vector3D (x1 * x2) (y1 * y2) (z1 * z2)
  abs (Vector3D x y z) = Vector3D (abs x) (abs y) (abs z)
  signum (Vector3D x y z) = Vector3D (signum x) (signum y) (signum z)
  fromInteger n = Vector3D (fromInteger n) (fromInteger n) (fromInteger n)
  negate (Vector3D x y z) = Vector3D (negate x) (negate y) (negate z)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) Int deriving (Show, Eq, Ord)

instance Functor Tree where
  fmap _ EmptyTree = EmptyTree
  fmap f (Node x left right h) = Node (f x) (fmap f left) (fmap f right) h

height :: Tree a -> Int
height EmptyTree = 0
height (Node _ _ _ h) = h

balanceFactor :: Tree a -> Int
balanceFactor EmptyTree = 0
balanceFactor (Node _ left right _) = height right - height left

updateHeight :: Tree a -> Tree a
updateHeight EmptyTree = EmptyTree
updateHeight (Node x left right _) = 
  Node x left right (1 + max (height left) (height right))

rotateLeft :: Tree a -> Tree a
rotateLeft EmptyTree = EmptyTree
rotateLeft (Node x left EmptyTree _) = updateHeight (Node x left EmptyTree 0)
rotateLeft (Node x left (Node y rl rr _) _) = 
  let newLeft = updateHeight (Node x left rl 0)
  in updateHeight (Node y newLeft rr 0)

rotateRight :: Tree a -> Tree a
rotateRight EmptyTree = EmptyTree
rotateRight (Node x EmptyTree right _) = updateHeight (Node x EmptyTree right 0)
rotateRight (Node y (Node x ll lr _) right _) = 
  let newRight = updateHeight (Node y lr right 0)
  in updateHeight (Node x ll newRight 0)

balance :: Tree a -> Tree a
balance EmptyTree = EmptyTree
balance tree@(Node x left right _)
  | bf > 1 && rightBf >= 0 = rotateLeft tree                                      -- Right-Right case
  | bf > 1 = let newRight = rotateRight right                                     -- Right-Left case
             in rotateLeft (updateHeight (Node x left newRight 0))
  | bf < -1 && leftBf <= 0 = rotateRight tree                                     -- Left-Left case
  | bf < -1 = let newLeft = rotateLeft left                                       -- Left-Right case
              in rotateRight (updateHeight (Node x newLeft right 0))
  | otherwise = updateHeight tree
  where
    bf = balanceFactor tree
    leftBf = balanceFactor left
    rightBf = balanceFactor right

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree 1

insertTree :: Ord a => a -> Tree a -> Tree a
insertTree x EmptyTree = singletonTree x
insertTree x node@(Node y left right _)
  | x < y     = let newLeft = balance $ insertTree x left
                in balance $ updateHeight $ Node y newLeft right 0
  | x > y     = let newRight = balance $ insertTree x right
                in balance $ updateHeight $ Node y left newRight 0
  | otherwise = node  -- Handle the case when x == y


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
tell [_] = "The list has only one element"
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
quicksort [] = [] -- base case
quicksort (p:xs) =
        let sortleft = filter (<= p) xs
            sortright = filter (> p) xs
        in quicksort sortleft ++ [p] ++ quicksort sortright

convertDigitToSum :: Int -> Int
convertDigitToSum = sum . map digitToInt . show . abs

findTo:: Int -> Maybe Int
findTo n = find (\x -> convertDigitToSum x == n) [1..]

findKey::(Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldl (\acc (k,v)-> if k == key then Just v else acc) Nothing

treeElem :: Ord a => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y left right _)
  | x < y     = treeElem x left
  | x > y     = treeElem x right
  | otherwise = True  -- x == y, found the element