module Main (main) where

import Lib

data Mood = Blah | Woot deriving Show
isPalindrome::(Eq a) =>[a] -> Bool
isPalindrome x = reverse x == x

changeMood::Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
changeMood _ = Blah

myGreeting = (++) "Hello" "World"
hello = "hello"
world = "world!"


main :: IO ()
main = do
    --putStrLn myGreeting
    --putStrLn (lessone secondGreet)
    --    where secondGreet = hello ++ world
    --          minone = length secondGreet - 1
    --          lessone str = take minone secondGreet
    --print (changeMood Blah)
    print (isPalindrome "abba")
