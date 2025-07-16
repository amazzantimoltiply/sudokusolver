module GameOfLife
    ( Grid
    , Cell
    , createGrid
    , nextGeneration
    , getCell
    ) where

type Cell = Bool  -- True for alive, False for dead
type Grid = [[Cell]]

createGrid :: Int -> Int -> Grid
createGrid rows cols = replicate rows (replicate cols False)

getCell :: Grid -> Int -> Int -> Cell
getCell grid row col
    | row < 0 || row >= length grid = False
    | col < 0 || col >= length (head grid) = False
    | otherwise = grid !! row !! col

countLiveNeighbors :: Grid -> Int -> Int -> Int
countLiveNeighbors grid row col = sum [fromEnum (getCell grid (row + dr) (col + dc)) |
    dr <- [-1..1],
    dc <- [-1..1],
    not (dr == 0 && dc == 0)]

nextGeneration :: Grid -> Grid
nextGeneration grid = [[newCellState grid row col | col <- [0..cols-1]] | row <- [0..rows-1]]
    where
        rows = length grid
        cols = length (head grid)
        newCellState g r c = 
            let neighbors = countLiveNeighbors g r c
                isAlive = getCell g r c
            in case (isAlive, neighbors) of
                (True, 2) -> True
                (True, 3) -> True
                (False, 3) -> True
                _ -> False
