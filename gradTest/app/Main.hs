module Main (main) where

import Lib

descend gradF iterN gamma x0 = take iterN (iterate step x0)
    where
        step x = x - gamma * gradF x
gradF_test x = 2 * (x-3) -- Gradient of f(x) = (x-3)^2

main :: IO ()
main = print (descend gradF_test 10 0.5 0.0)
