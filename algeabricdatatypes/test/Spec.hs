import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "isCar" $ do
    it "returns True for a Car" $ do
      isCar (Car (Price 10000)) `shouldBe` True

    it "returns False for a Plane" $ do
      isCar (Plane (Price 1000000)) `shouldBe` False

  describe "areCars" $ do
    it "returns True for an empty list" $ do
      areCars [] `shouldBe` True

    it "returns True for a list containing only Cars" $ do
      areCars [Car (Price 10000), Car (Price 20000)] `shouldBe` True

    it "returns False for a list containing at least one Plane" $ do
      areCars [Car (Price 10000), Plane (Price 1000000), Car (Price 20000)] `shouldBe` False

    it "returns False for a list containing only Planes" $ do
      areCars [Plane (Price 1000000), Plane (Price 2000000)] `shouldBe` False
