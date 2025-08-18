module LibSpec (spec) where
import Lib (primes, upto, countDoubled, reverseList, reverseListRecursive)
import Test.Hspec

spec :: Spec
spec = do
  describe "primes" $ do
    it "returns the first few prime numbers correctly" $ do
      (take 10 primes) `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]

    it "correctly identifies larger prime numbers" $ do
      (takeWhile (< 100) primes) `shouldBe` [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
    it "takes numbers up to n from a list" $ do
      (upto 5 [1..10] :: [Int]) `shouldBe` [1, 2, 3, 4, 5]
    
    it "handles empty lists" $ do
      (upto 5 [] :: [Int]) `shouldBe` []
    
    it "handles lists with all elements greater than n" $ do
      (upto 5 [6..10] :: [Int]) `shouldBe` []
    
    it "handles lists with all elements less than or equal to n" $ do
      (upto 10 [1..5] :: [Int]) `shouldBe` [1..5]
    
    it "works with floating point numbers" $ do
      upto (3.5 :: Double) [1.0, 2.0, 3.0, 4.0] `shouldBe` [1.0, 2.0, 3.0]
  describe "counting words inside strings" $ do
    it "count double words inside a string" $ do
      countDoubled "aannddrreea" `shouldBe` 5

  describe "reverseList" $ do
    it "reverses an empty list" $ do
      reverseList ([] :: [Int]) `shouldBe` []
    
    it "reverses a single element list" $ do
      (reverseList [1] :: [Int]) `shouldBe` [1]
    
    it "reverses a list of numbers" $ do
      (reverseList [1..5] :: [Int]) `shouldBe` [5,4,3,2,1]
    
    it "reverses a string" $ do
      reverseList "hello" `shouldBe` "olleh"
    
    it "maintains list length" $ do
      length (reverseList ([1..100] :: [Int])) `shouldBe` 100
      
  describe "reverseList implementations comparison" $ do
    it "both implementations give same results for empty list" $ do
      (reverseList ([] :: [Int])) `shouldBe` reverseListRecursive []
    
    it "both implementations give same results for single element" $ do
      (reverseList [1] :: [Int]) `shouldBe` reverseListRecursive [1]
    
    it "both implementations give same results for list of numbers" $ do
      (reverseList [1..5] :: [Int]) `shouldBe` reverseListRecursive [1..5]
    
    it "both implementations give same results for strings" $ do
      reverseList "hello" `shouldBe` reverseListRecursive "hello"
    
    it "both implementations maintain list length for large lists" $ do
      length (reverseList ([1..1000] :: [Int])) `shouldBe` length (reverseListRecursive ([1..1000] :: [Int]))