module Lib
    ( 
    , Vehicle(..)
    , Car(..)
    , Plane(..)
    , Price(..)
    , isCar
    , areCars
    ) where


data Price = Price Int deriving (Show, Eq)

data Vehicle = Car Price | Plane Price deriving (Show, Eq)
data Car = Punto | Focus deriving (Show, Eq)
data Plane = Boeing Int | Airbus Int deriving (Show, Eq)

isCar :: Vehicle -> Bool
isCar (Car _) = True
isCar _       = False

areCars :: [Vehicle] -> Bool
areCars = all isCar
