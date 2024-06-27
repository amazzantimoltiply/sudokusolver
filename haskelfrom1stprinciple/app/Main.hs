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

funcC::(Ord a)=>a->a->Bool
funcC x y 
  | x < y = True
  | otherwise = False 

fstString::[Char]->[Char]
fstString x = x ++ " in the rain"

sndString::[Char]->[Char]
sndString x = x ++ " over the raombow"

sing = if x < y then fstString x else sndString y
    where x = "Singing"
          y = "Somewhere"

f::Int->String
f = undefined
g::String->Char
g = undefined

h::Int->Char
h i = g (f i)

data A
data B
data C

q::A->B
q=undefined

w::B->C
w=undefined

e::A->C
e a = w (q a)


main :: IO ()
main = do
    --putStrLn myGreeting
    --putStrLn (lessone secondGreet)
    --    where secondGreet = hello ++ world
    --          minone = length secondGreet - 1
    --          lessone str = take minone secondGreet
    --print (changeMood Blah)
    --print (isPalindrome "abba")
    print sing
