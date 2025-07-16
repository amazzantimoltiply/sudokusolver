module Main (main) where

import Test.Hspec
import Data.Bifunctor (bimap)
import Lib (MyEither(..), Countable(..), safeDivide,
            Card(..), CardValue(..), CardSuite(..), fullDeck, smallDeck,
            Person(..), tell,loadFromFile,quicksort,trim',groupPairs,convertDigitToSum)

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