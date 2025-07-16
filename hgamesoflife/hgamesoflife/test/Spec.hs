module Main (main) where

import Test.Hspec
import Lib
import Data.Map (toList, fromList)
import System.Directory (removeFile)
import Control.Exception (catch, SomeException)

-- Helper function to clean up test files
cleanupFile :: FilePath -> IO ()
cleanupFile path = catch (removeFile path) (\(_ :: SomeException) -> pure ())

main :: IO ()
main = hspec $ do
  describe "Board Loading and Saving" $ do
    describe "dimensions" $ do
      it "calculates correct dimensions for empty board" $ do
        let (_, dims) = loadBoard ""
        width dims `shouldBe` 0
        height dims `shouldBe` 0

      it "calculates correct dimensions for single line" $ do
        let (_, dims) = loadBoard ".#."
        width dims `shouldBe` 3
        height dims `shouldBe` 1

      it "calculates correct dimensions for multi-line board" $ do
        let input = unlines [ ".#."
                          , "..#"
                          , "###"
                          ]
        let (_, dims) = loadBoard input
        width dims `shouldBe` 3
        height dims `shouldBe` 3

    describe "cell loading" $ do
      it "loads empty board correctly" $ do
        let (board, _) = loadBoard "..."
        toList board `shouldBe` []

      it "loads single live cell correctly" $ do
        let (board, _) = loadBoard "#"
        toList board `shouldMatchList` [((0,0), Alive)]

      it "loads multiple live cells correctly" $ do
        let (board, _) = loadBoard "#.#"
        toList board `shouldMatchList` [((0,0), Alive), ((2,0), Alive)]

      it "loads multi-line pattern correctly" $ do
        let input = unlines [ ".#."  -- Generation 1 glider
                          , "..#"
                          , "###"
                          ]
        let (board, _) = loadBoard input
        toList board `shouldMatchList` 
          [ ((1,0), Alive)  -- .#.
          , ((2,1), Alive)  -- ..#
          , ((0,2), Alive)  -- ###
          , ((1,2), Alive)
          , ((2,2), Alive)
          ]

    describe "edge cases" $ do
      it "handles empty lines" $ do
        let input = "\n\n\n"
        let (board, dims) = loadBoard input
        width dims `shouldBe` 0
        height dims `shouldBe` 3
        toList board `shouldBe` []

      it "handles different line lengths" $ do
        let input = unlines [ "#"      -- width 1
                          , "##"     -- width 2
                          , "###"    -- width 3
                          ]
        let (board, dims) = loadBoard input
        width dims `shouldBe` 3  -- Takes the longest line
        height dims `shouldBe` 3
        toList board `shouldMatchList` 
          [ ((0,0), Alive)
          , ((0,1), Alive), ((1,1), Alive)
          , ((0,2), Alive), ((1,2), Alive), ((2,2), Alive)
          ]

      it "handles trailing newlines" $ do
        let (board, dims) = loadBoard "#.#\n"
        width dims `shouldBe` 3
        height dims `shouldBe` 1
        toList board `shouldMatchList` [((0,0), Alive), ((2,0), Alive)]

    describe "specific patterns" $ do
      it "loads the standard glider pattern correctly" $ do
        let input = unlines [ ".#."
                          , "..#"
                          , "###"
                          ]
        let (board, _) = loadBoard input
        toList board `shouldMatchList` toList glider

      it "loads blinker pattern correctly" $ do
        let input = unlines [ "..."
                          , "###"
                          , "..."
                          ]
        let (board, _) = loadBoard input
        toList board `shouldMatchList` 
          [ ((0,1), Alive)
          , ((1,1), Alive)
          , ((2,1), Alive)
          ]

    describe "boardToString" $ do
      it "converts empty board correctly" $ do
        let board = fromList []
        let dims = BoardDimensions 2 2
        boardToString board dims `shouldBe` "..\n..\n"

      it "converts single cell board correctly" $ do
        let board = fromList [((0,0), Alive)]
        let dims = BoardDimensions 2 2
        boardToString board dims `shouldBe` "#.\n..\n"

      it "converts glider pattern correctly" $ do
        boardToString glider (BoardDimensions 3 3) `shouldBe` ".#.\n..#\n###\n"

    describe "save and load" $ do
      it "can save and load empty board" $ do
        let path = "test_empty.txt"
        let board = fromList []
        let dims = BoardDimensions 2 2
        
        -- Clean up any existing file
        cleanupFile path
        
        -- Save board
        result <- saveBoard path board dims
        result `shouldBe` Right ()
        
        -- Load and verify
        (loadedBoard, loadedDims) <- loadBoard <$> readFile path
        loadedDims `shouldBe` dims
        toList loadedBoard `shouldBe` toList board
        
        -- Clean up
        cleanupFile path

      it "can save and load glider pattern" $ do
        let path = "test_glider.txt"
        cleanupFile path
        
        -- Save glider
        result <- saveBoard path glider (BoardDimensions 3 3)
        result `shouldBe` Right ()
        
        -- Load and verify
        (loadedBoard, loadedDims) <- loadBoard <$> readFile path
        loadedDims `shouldBe` BoardDimensions 3 3
        toList loadedBoard `shouldMatchList` toList glider
        
        -- Clean up
        cleanupFile path

      it "handles file write errors" $ do
        result <- saveBoard "/invalid/path/file.txt" glider (BoardDimensions 3 3)
        case result of
          Left err -> err `shouldContain` "Error writing file"
          Right _ -> expectationFailure "Expected error for invalid file path"

      it "maintains board dimensions through save/load cycle" $ do
        let path = "test_dims.txt"
        let board = fromList [((0,0), Alive), ((2,2), Alive)]
        let dims = BoardDimensions 3 3  -- Explicitly larger than minimal
        
        cleanupFile path
        
        -- Save board
        result <- saveBoard path board dims
        result `shouldBe` Right ()
        
        -- Load and verify dimensions are preserved
        (_, loadedDims) <- loadBoard <$> readFile path
        loadedDims `shouldBe` dims
        
        -- Clean up
        cleanupFile path
        
      it "preserves all cell states" $ do
        let path = "test_states.txt"
        let board = fromList [((0,0), Alive), ((1,0), Dead), ((0,1), Dead), ((1,1), Alive)]
        let dims = BoardDimensions 2 2
        
        cleanupFile path
        
        -- Save board
        result <- saveBoard path board dims
        result `shouldBe` Right ()
        
        -- Load and verify
        contents <- readFile path
        contents `shouldBe` "#.\n.#\n"
        
        -- Clean up
        cleanupFile path
