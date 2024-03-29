module Lib where

addone::Int->Int
addone x = x + 1
power2::Int->Int
power2 x = x * x
doubleinc::Int->Int
doubleinc = addone . addone

greatings = (<>) . (<> " ")
greetings str1 str2 = str1 ++ " " ++ str2


infixl 7 +++
(+++) a b = a + b

makeGreet::String->String->String
makeGreet msg1 msg2 =
    let msgwithspace = msg1 <> " " in msgwithspace <> msg2

printSmallNum::Int->IO()
printSmallNum num =
    let msg
            | num < 10 = show num
            | otherwise ="Number must be <= than 10"
    in print msg

fizzBuzzFor::Int->String
fizzBuzzFor num
    |0 == rem num 15 = "fizzbuzz"
    |0 == rem num 5  = "buzz"
    |0 == rem num 3  = "fizz"
    |otherwise = show num

fizzBuzz finalNum curNum message =
    if curNum > finalNum then message
    else 
        let nextMessage = message <> fizzBuzzFor curNum <> " "
            nextNum = curNum + 1
        in fizzBuzz finalNum nextNum nextMessage
-- exercise
fact::Int->Int
fact num = 
    if num == 0 then 1
    else
        let decnum = num - 1
        in num * fact decnum 
fibonacci::Int->Int
fibonacci n 
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fibonacci (n-1) + fibonacci (n-2) 
tails'::[a]->[[a]]
tails' [] = []
tails' (_:xs) = xs : tails' (xs)