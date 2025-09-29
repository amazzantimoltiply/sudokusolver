{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lib
  ( Digit,
    Row,
    Grid,
    Matrix,
    choice,
    choices,
    digits,
    rows,
    cols,
    cp,
    cb,
    expand,
    completitions,
    group,
    ungroup,
    chop,
    boxs,
    pack,
    valid,
    nodups,
    solve,
    Username (..),
    AccountNumber (..),
    User (..),
    printUser,
    EmployeeRank (..),
    rank,
    devidedBy,
    Expr (..),
    renderExpr,
    eval,
    selectWords,
  )
where

import Data.List

-- | Type aliases for clarity
type Digit = Char

type Row a = [a]

type Grid = Matrix Digit

type Matrix a = [Row a]

-- | The valid digits that can appear in the grid
digits :: [Digit]
digits = ['1' .. '9']

-- | For a given cell, return the possible values
choice :: Digit -> [Digit]
choice '0' = digits -- for blank cell, all digits are possible
choice c = [c] -- for filled cell, only that digit is possible

-- | Convert a grid into a grid of choice lists
choices :: Grid -> Matrix [Digit]
choices = map (map choice)

-- | Get rows from a grid (identity function as grid is stored as rows)
rows :: Grid -> Grid
rows = id

-- cols                  :: Matrix a -> [Row a]
-- cols                  =  transpose

-- | Get columns from a grid (transpose)
cols :: Matrix a -> [Row a]
cols [] = []
cols [xs] = map (: []) xs
cols (xs : xss) = zipWith (:) xs (cols xss)

-- | Cartesian product of a list of lists
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp [xs] = [[x] | x <- xs] -- Optimization for single list case
cp (xs : xss)
  | null xs = [] -- Short-circuit when any list is empty
  | otherwise = [x : ys | x <- xs, ys <- cp xss]

-- | Cartesian block - combines elements from blocks
cb :: [[a]] -> [[a]]
cb [] = [[]]
cb [xs] = [[x] | x <- xs]
cb (xs : xss) = [x : ys | x <- xs, ys <- cb xss]

expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

completitions = expand . choices

-- | Group a list into sublists of given size
-- Returns Nothing if n <= 0, Just result otherwise
-- group :: [a]->[[a]]
-- group [] = []
-- group xs = take 2 xs : group (drop 2 xs)

-- | Flatten a list of lists into a single list
boxsize = 2

ungroup :: [[a]] -> [a]
ungroup = concat

pack :: [[a]] -> [[[[a]]]]
pack = split . map split

split :: [a] -> [[a]]
split = chop boxsize

unpack :: [[[[a]]]] -> [[a]]
unpack = map concat . concat

boxs :: Matrix a -> [Row a]
boxs = unpack . map cols . pack

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

valid :: Grid -> Bool
valid g =
  all nodups (rows g)
    && all nodups (cols g)
    && all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = notElem x xs && nodups xs

solve = filter valid . expand . choices

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser Username | RegisteredUser Username AccountNumber

printUser :: User -> String
printUser (UnregisteredUser (Username name)) = "Unregistered user: " ++ name
printUser (RegisteredUser (Username name) (AccountNumber accNum)) =
  name ++ " has account number: " ++ show accNum

data EmployeeRank = Intern | Developer | Manager | Director | VP | CEO deriving (Eq, Ord, Show)

rank :: EmployeeRank -> EmployeeRank -> Ordering
rank = compare

isLeapYear :: Integer -> Bool
isLeapYear year = (isDivBy4 year && isNotDivBy100 year) || isDivBy400 year
  where
    isDivBy4 y = mod y 4 == 0
    isNotDivBy100 y = mod y 100 /= 0
    isDivBy400 y = mod y 400 == 0

devidedBy :: (Integral a) => a -> a -> (a, a)
devidedBy num den = go num den 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - 1) d (count + 1)

data Expr = X | Y | Lit Integer | Add Expr Expr deriving (Show, Eq)

eval :: Expr -> Integer -> Integer -> Integer
eval X x _ = x
eval Y _ y = y
eval (Lit n) _ _ = n
eval (Add e1 e2) x y = eval e1 x y + eval e2 x y

renderExpr :: Expr -> String
renderExpr X = "x"
renderExpr Y = "y"
renderExpr (Lit n) = show n
renderExpr (Add e1 e2) = renderExpr e1 ++ "+" ++ renderExpr e2

selectWords :: String -> [String]
selectWords s
  | null s = []
  | all (== ' ') s = []
  | otherwise = word : selectWords rest
  where
    word = takeWhile (/= ' ') s
    rest = dropWhile (== ' ') $ dropWhile (/= ' ') s

selectWords' s = [[x] | x <- takeWhile (/= ' ') s]
