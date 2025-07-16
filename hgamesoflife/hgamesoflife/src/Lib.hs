module Lib
    ( glider
    , Coords
    , Board
    , Cell(..)
    , loadBoard
    , saveBoard
    , boardToString  -- export for testing
    ) where

import Data.Map (Map, fromList, toList, findWithDefault)
import Control.Exception (try)
import Data.List (intercalate)

type Coords = (Int, Int)
data Cell = Alive | Dead deriving (Show, Eq)
type Board = Map Coords Cell

-- | A glider pattern
-- .#.
-- ..#
-- ###
glider :: Board
glider = fromList [((1,0), Alive),   -- .#.
                   ((2,1), Alive),    -- ..#
                   ((0,2), Alive),    -- ###
                   ((1,2), Alive),
                   ((2,2), Alive)]

-- | Load a board from a string representation where:
-- '#' represents a live cell
-- '.' represents a dead cell
-- Returns the board, and its width and height
-- Example:
-- > let (board, w, h) = loadBoard ".#.\n..#\n###\n"
-- > w  -- returns 3
-- > h -- returns 3
loadBoard :: String -> (Board, Int, Int)
loadBoard input = 
    let rows = lines input
        h = length rows
        w = if h == 0 then 0 else maximum (map length rows)
        board = fromList [(((x, y), Alive)) | 
                         (y, line) <- zip [0..] rows,
                         (x, c) <- zip [0..] line,
                         c == '#']
    in (board, w, h)

-- | Save a board to a file using '#' for live cells and '.' for dead cells
-- Returns Left String on error, Right () on success
saveBoard :: FilePath -> Board -> Int -> Int -> IO (Either String ())
saveBoard path board w h = do
    let content = boardToString board w h
    result <- try (writeFile path content) :: IO (Either IOError ())
    pure $ case result of 
        Left err -> Left $ "Error writing file: " ++ show err
        Right _ -> Right ()

-- | Convert a board to a string representation
boardToString :: Board -> Int -> Int -> String
boardToString board w h = 
    unlines [ [ cellToChar (findWithDefault Dead (x, y) board) | x <- [0..w-1] ] | y <- [0..h-1] ]
  where
    cellToChar Alive = '#'
    cellToChar Dead = '.'

