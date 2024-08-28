module Main (main) where

data Price = Price Integer deriving (Eq,Show)
data Manufacturer = FCA | Ferrari | Lamborghini deriving (Eq,Show)
data Airline = Ita | Airfrance | Ryanair deriving (Eq,Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq,Show)

myCar = Car Ferrari (Price 400000)
urCar = Car FCA (Price 8000)
myPlane = Ita

isFerrari::Vehicle->Bool
isFerrari (Car Ferrari _ ) = True
isFerrari _ = False

main :: IO ()
main = do
    print (isFerrari (Car Ferrari (Price 10000) ))



