module Lib(
    render, Outcome, Person, age,
) where

data Outcome = Lose | Draw | Win
data Person = MkPerson String Int
render::Outcome->String
render Lose = "Lose"
render Draw = "Draw"
render Win = "Win"

age::Person -> Int
age (MkPerson n a) = a