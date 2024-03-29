import Data.Char
import Data.List
import System.IO

size::Int
size=3

data Player= O|B|X deriving (Eq,Ord,Show)
type Grid = [[Player]]

next::Player -> Player
next O = X
next B = B
next X = O

empty::Grid
empty = replicate size (replicate size B)

full::Grid->Bool
full = all (/= B) . concat

turn::Grid -> Player
turn g = if os <= xs then O else X
             where
                os = length (filter (== O) ps)
                xs = length (filter (== X) ps)
                ps = concat g