module Main where
import Lib
import Data.Maybe
main :: IO()
main = do
    let str="Hello"
    do print $ listLenght str
    do print $ Data.Maybe.fromMaybe (-1) (indexOf 'f' str 0)
    do print $ charByPos 0 "Andrea"
    do print $ upperRot 4 'Z'
    do print $ rotChar 'a' 3
    do print $ caesar 13 (caesar 13 "Andrea")
    do print $ transform (\x-> x +1) [1..5]
    do print $ count 'a' "Andreaaaa"
    return ()
