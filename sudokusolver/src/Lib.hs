module Lib
    ( Digit
    , Row
    , Grid
    , Matrix
    , choice
    , choices
    , digits
    , rows
    , cols
    , cp
    , cb
    , expand
    , completitions
    , group
    , ungroup
    ) where

-- | Type aliases for clarity
type Digit = Char
type Row a = [a]
type Grid = Matrix Digit
type Matrix a = [Row a]

-- | The valid digits that can appear in the grid
digits :: [Digit]
digits = ['1'..'9']

-- | For a given cell, return the possible values
choice :: Digit -> [Digit]
choice '0' = digits  -- for blank cell, all digits are possible
choice c   = [c]     -- for filled cell, only that digit is possible

-- | Convert a grid into a grid of choice lists
choices :: Grid -> Matrix [Digit]
choices = map (map choice)

-- | Get rows from a grid (identity function as grid is stored as rows)
rows :: Grid -> Grid
rows = id

-- | Get columns from a grid (transpose)
cols :: Matrix a-> [Row a]
cols [] = []
cols [xs] = [xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

-- | Cartesian product of a list of lists
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp [xs] = [[x] | x <- xs]  -- Optimization for single list case
cp (xs:xss)
    | null xs = []         -- Short-circuit when any list is empty
    | otherwise = [x:ys | x <- xs, ys <- cp xss]

-- | Cartesian block - combines elements from blocks
cb :: [[a]] -> [[a]]
cb [] = [[]]
cb [xs] = [[x] | x <- xs]
cb (xs:xss) = [x:ys | x <- xs, ys <- cb xss]

expand::Matrix [Digit]-> [Grid]
expand = cp . map cp

completitions = expand . choices

-- | Group a list into sublists of given size
-- Returns Nothing if n <= 0, Just result otherwise
group :: [a]->[[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)


-- | Flatten a list of lists into a single list
ungroup :: [[a]] -> [a]
ungroup = concat

boxs = group  . map group
