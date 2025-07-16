module Main where

import Test.Hspec
import GameOfLife

main :: IO ()
main = hspec $ do
    describe "Game of Life" $ do
        it "creates an empty grid of specified size" $ do
            let grid = createGrid 2 3
            length grid `shouldBe` 2
            length (head grid) `shouldBe` 3
            all (not . or) grid `shouldBe` True

        it "counts live neighbors correctly" $ do
            let grid = [[False, True, False],
                       [False, True, False],
                       [False, True, False]]
            getCell grid 1 1 `shouldBe` True
            nextGeneration grid !! 1 !! 1 `shouldBe` False  -- Dies from underpopulation
