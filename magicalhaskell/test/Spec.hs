module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Data.Bifunctor (bimap)
import qualified Data.Map as Map

import Lib (MyEither(..), Countable(..), safeDivide,
            Card(..), CardValue(..), CardSuite(..), fullDeck, smallDeck,
            Person(..), tell,
            loadFromFile,
            quicksort,
            trim',
            groupPairs,
            convertDigitToSum,
            findTo,
            findKey, Vector3D(..),
            List(..), 
            toList, 
            fromList,
            Tree(..),
            singletonTree,
            insertTree,
            treeElem,
            height,
            balanceFactor)

main :: IO ()
main = hspec $ do
  describe "MyEither" $ do
    it "MyLeft holds the left value" $ do
      let v = MyLeft "err" :: MyEither String Int
      case v of
        MyLeft s -> s `shouldBe` "err"
        _        -> expectationFailure "Expected MyLeft"

    it "MyRight holds the right value" $ do
      let v = MyRight 42 :: MyEither String Int
      case v of
        MyRight n -> n `shouldBe` 42
        _         -> expectationFailure "Expected MyRight"

    it "fmap over MyLeft does nothing" $ do
      fmap (+1) (MyLeft "fail" :: MyEither String Int) `shouldBe` MyLeft "fail"

    it "fmap over MyRight applies the function" $ do
      fmap (+1) (MyRight 1 :: MyEither String Int) `shouldBe` MyRight 2

    it "safeDivide returns MyRight for normal division" $ do
      safeDivide (6 :: Double) 2 `shouldBe` MyRight 3.0
      safeDivide (7 :: Double) 2 `shouldBe` MyRight 3.5

    it "safeDivide returns MyLeft for division by zero" $ do
      safeDivide (5 :: Double) 0 `shouldBe` MyLeft "Division by zero"
      safeDivide (0 :: Double) 0 `shouldBe` MyLeft "Division by zero"

    describe "Bifunctor MyEither" $ do
      it "bimap applies function to MyLeft" $ do
        bimap (++" error") (+ (1 :: Integer)) (MyLeft "some" :: MyEither String Integer) `shouldBe` MyLeft "some error"
      it "bimap applies function to MyRight" $ do
        bimap (++" error") (+ (1 :: Integer)) (MyRight 10 :: MyEither String Integer) `shouldBe` MyRight 11
      it "bimap does not change the other side" $ do
        bimap (++" error") (+ (1 :: Integer)) (MyRight 0 :: MyEither String Integer) `shouldBe` MyRight 1
        bimap (++" error") (+ (1 :: Integer)) (MyLeft "fail" :: MyEither String Integer) `shouldBe` MyLeft "fail error"
  
  describe "Countable" $ do
    it "fmap increments count and applies function" $ do
      let c = Countable 0 10 :: Countable Int
      fmap (+1) c `shouldBe` Countable 1 11

    it "Applicative Countable works as expected" $ do
      let c1 = Countable 2 ((+1) :: Int -> Int) :: Countable (Int -> Int)
          c2 = Countable 3 (4 :: Int) :: Countable Int
      (c1 <*> c2) `shouldBe` Countable 5 5
      liftA2 (+) (Countable 2 (3 :: Int)) (Countable 3 (4 :: Int)) `shouldBe` Countable 5 7

    it "Monad instance works correctly" $ do
      let c1 = Countable 1 (5 :: Integer)
          f x = Countable 2 (x * 2)
      (c1 >>= f) `shouldBe` Countable 3 10
  
  describe "Card" $ do
    it "shows cards correctly" $ do
      let card = Card Two Club
      show card `shouldBe` "2 of ♣"
    
    it "shows full deck correctly" $ do
      let deck = fullDeck
      length deck `shouldBe` 52
      show (head deck) `shouldBe` "2 of ♣"
    
    it "shows small deck correctly" $ do
      let deck = smallDeck
      length deck `shouldBe` 16
      show (head deck) `shouldBe` "2 of ♣"
  
  describe "Person" $ do
    it "shows person correctly" $ do
      let person = Person "Alice" 30
      show person `shouldBe` "Person {name = \"Alice\", age = 30}"

    it "load person from file returns one person" $ do
      let filePath = "person.txt"
      people <- loadFromFile filePath
      people `shouldBe` [Person "Alice" 30]

    it "load person from empty file returns []" $ do
      let filePath = "test/person.txt"
      people <- loadFromFile filePath
      people `shouldBe` []

    it "load multiple people from file returns all valid people" $ do
      let filePath = "test/multi_person.txt"
      people <- loadFromFile filePath
      people `shouldBe` [Person "Bob" 25, Person "Carol" 40]

    it "load file with invalid and valid people returns only valid people" $ do
      let filePath = "test/invalid_person.txt"
      people <- loadFromFile filePath
      people `shouldBe` [Person "Eve" 35]
    

  describe "talking with an agent" $ do
    it "can answer to a question" $ do
      let agentResponse = tell "Hello, how can I assist you today?"
      agentResponse `shouldBe` "Hello, how can I assist you today?"
      -- This is a placeholder for actual interaction logic
      -- In a real scenario, you would implement the logic to interact with an agent
      -- and verify the response.
    it "can handle empty input string" $ do 
      let agentResponse = tell ""
      agentResponse `shouldBe` "The list is empty"      -- This is a placeholder for actual interaction logic
      -- In a real scenario, you would implement the logic to interact with an agent
      -- and verify the response.
    it "can tell if the list has one element" $ do
      let agentResponse = tell "H"
      agentResponse `shouldBe` "The list has only one element"  -- This is a placeholder for actual interaction logic
      -- In a real scenario, you would implement the logic to interact with an agent
      -- and verify the response.
  describe "Quicksort algo" $ do
    it "sorts a list of integers" $ do
      let unsorted = [3, 1, 4, 1, 5, 9, 2, 6, 5]
          sorted = quicksort unsorted
      sorted `shouldBe` [1, 1, 2, 3, 4, 5, 5, 6, 9]

  describe "trim function" $ do
    it "trims whitespace from both ends of a string" $ do
      let str = "   Hello, World!\n   "
          trimmed = trim' str
      trimmed `shouldBe` "Hello, World!"

    it "trims newline characters from the end of a string" $ do
      let str = "Hello, World!\n"
          trimmed = trim' str
      trimmed `shouldBe` "Hello, World!"

    it "returns an empty string when input is empty" $ do
      let str = ""
          trimmed = trim' str
      trimmed `shouldBe` ""
  
  describe "groupPairs function" $ do
    it "groups a list into pairs" $ do
      let xs = [1, 2, 3, 4, 5, 6]
          pairs = groupPairs xs
      pairs `shouldBe` [(1, 2), (3, 4), (5, 6)]

    it "returns an empty list for an empty input" $ do
      let xs = []
          pairs = groupPairs xs
      pairs `shouldBe` ([] :: [(Int, Int)])

    it "handles odd-length lists by ignoring the last element" $ do
      let xs = [1, 2, 3, 4, 5]
          pairs = groupPairs xs
      pairs `shouldBe` [(1, 2), (3, 4)] 
    
  describe "convert digits to sum" $ do
    it "converts a digit to its sum representation" $ do
      let digits = 123
          sumRepresentation = convertDigitToSum digits
      sumRepresentation `shouldBe` 6
    it "converts a single digit to its sum representation" $ do
      let digits = 5
          sumRepresentation = convertDigitToSum digits
      sumRepresentation `shouldBe` 5  
    it "converts a negative digit to its sum representation" $ do
      let digits = -123
          sumRepresentation = convertDigitToSum digits
      sumRepresentation `shouldBe` 6

  describe "find the sum n of combined digits" $ do
    it "finds the sum of combined digits" $ do
      let n = 40
      let sum = findTo n
      sum `shouldBe` Just 49999
  
  describe "find a key in a phone book" $ do
    it "finds a key in the phone book" $ do
      let phoneBook = [("John", "123-4567")]
      let key = "John"
          phoneNumber = lookup key phoneBook
      phoneNumber `shouldBe` Just "123-4567"

    it "returns Nothing for a non-existent key" $ do
      let phoneBook = [("John", "123-4567")]
      let key = "NonExistent"
          phoneNumber = lookup key phoneBook
      phoneNumber `shouldBe` Nothing

    it "returns Just for an existing key" $ do
      let phoneBook = [("John", "123-4567"),
                       ("Jane", "987-6543")]
      let key = "Jane"
          phoneNumber = lookup key phoneBook
      phoneNumber `shouldBe` Just "987-6543"
  describe "findKey function" $ do
    it "finds a key in a list of pairs" $ do
      let phoneBook = [("John", "123-4567"), ("Jane", "987-6543")]
      findKey "John" phoneBook `shouldBe` Just "123-4567"

    it "returns Nothing for a non-existent key" $ do
      let phoneBook = [("John", "123-4567")]
      findKey "NonExistent" phoneBook `shouldBe` Nothing

    it "returns Just for an existing key" $ do
      let phoneBook = [("Alice", "555-1234"), ("Bob", "555-5678")]
      findKey "Alice" phoneBook `shouldBe` Just "555-1234"
  describe "using lookup" $ do
    it "finds a key in a list of pairs" $ do
      let phoneBook = [("John", "123-4567"), ("Jane", "987-6543")]
      lookup "John" phoneBook `shouldBe` Just "123-4567"

    it "returns Nothing for a non-existent key" $ do
      let phoneBook = [("John", "123-4567")]
      lookup "NonExistent" phoneBook `shouldBe` Nothing

    it "returns Just for an existing key" $ do
      let phoneBook = Map.fromList [("Alice", "555-1234"), ("Bob", "555-5678")]
      let updatedBook = Map.insert "Andrea" "555-4455" phoneBook   
      Map.lookup "Andrea" updatedBook `shouldBe` Just "555-4455"
  
  describe "test algeabric operators on vectors" $ do
    it "can add two vectors" $ do
      let v1 = Vector3D 1 2 3
          v2 = Vector3D 4 5 6
      let result = v1 + v2
      result `shouldBe` Vector3D 5 7 9

  describe "my list operator" $ do
    it "can create custom lists" $ do
      let myList = fromList [3, 4, 5] :: List Int
      toList myList `shouldBe` [3, 4, 5]

    it "can convert from regular list" $ do
      let regularList = [3, 4, 5]
          customList = fromList regularList :: List Int
      toList customList `shouldBe` regularList

  describe "AVL Tree operations" $ do
    it "maintains BST property" $ do
      let values = [5, 3, 7, 1, 9, 4, 6] :: [Int]
          tree = foldr insertTree EmptyTree values
      all (\x -> treeElem x tree) values `shouldBe` True  -- Contains all inserted elements
      treeElem 8 tree `shouldBe` False                    -- Doesn't contain non-inserted elements

    it "maintains AVL balance invariant" $ do
      let values = [5, 3, 7, 1, 9, 4, 6] :: [Int]
          tree = foldr insertTree EmptyTree values
      abs (balanceFactor tree) `shouldSatisfy` (<= 1)
      all (\x -> treeElem x tree) values `shouldBe` True

    it "handles increasing sequence correctly" $ do
      let values = [1..7] :: [Int]
          tree = foldr insertTree EmptyTree values
      abs (balanceFactor tree) `shouldSatisfy` (<= 1)
      all (\x -> treeElem x tree) values `shouldBe` True

    it "handles decreasing sequence correctly" $ do
      let values = [7,6..1] :: [Int]
          tree = foldr insertTree EmptyTree values
      abs (balanceFactor tree) `shouldSatisfy` (<= 1)
      all (\x -> treeElem x tree) values `shouldBe` True

    it "handles duplicates correctly" $ do
      let values = [5,5,5,5] :: [Int]
          tree = foldr insertTree EmptyTree values
      height tree `shouldBe` 1  -- Only one node should exist
      treeElem 5 tree `shouldBe` True

    it "maintains correct height after operations" $ do
      let values = [5,3,7,1,9,4,6] :: [Int]
          tree = foldr insertTree EmptyTree values
          n = length values
      putStrLn $ "Tree: " ++ show tree
      putStrLn $ "Height: " ++ show (height tree)
      putStrLn $ "Expected max height: " ++ show (ceiling (logBase 2 (fromIntegral n + 1)))
      putStrLn $ "Insertions: " ++ show (reverse values)
      height tree `shouldSatisfy` (<= ceiling (logBase 2 (fromIntegral n + 1)))

  describe "Functor instance for AVL Tree" $ do
    it "maintains AVL properties after fmap" $ do
      let values = [5,3,7,1,9,4,6] :: [Int]
          tree = foldr insertTree EmptyTree values
          mappedTree = fmap (*2) tree
      abs (balanceFactor mappedTree) `shouldSatisfy` (<= 1)
      height mappedTree `shouldBe` height tree

    it "preserves tree structure after fmap" $ do
      let values = [5,3,7] :: [Int]
          tree = foldr insertTree EmptyTree values
          mappedTree = fmap (*2) tree
      height mappedTree `shouldBe` height tree
      abs (balanceFactor mappedTree) `shouldSatisfy` (<= 1)