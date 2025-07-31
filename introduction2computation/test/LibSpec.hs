module LibSpec (spec) where
import Lib (primes,upto)
import Test.Hspec
import Lib

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
    