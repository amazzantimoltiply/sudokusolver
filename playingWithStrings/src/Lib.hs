module Lib
    ( mylines,myUnlines
    ) where


mylines::[Char]->[[Char]]
mylines [] = []
mylines xs =
    let
    beforenl::[Char]->[Char]
    beforenl [] = ""
    beforenl (x:xs)
            | x == '\n' = ""
            | otherwise = x : beforenl xs
    afternl::[Char]->[Char]
    afternl [] = ""
    afternl (x:xs)
            | x == '\n' = xs
            | otherwise = afternl xs
    in beforenl xs : mylines (afternl xs)

myUnlines::[[Char]]->[Char]
myUnlines [x] = x
myUnlines [] = ""
myUnlines (x:xs) = x ++ "\n" ++ myUnlines xs