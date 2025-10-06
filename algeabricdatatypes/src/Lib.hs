module Lib
    ( Vehicle(..)
    , Car(..)
    , Plane(..)
    , Price(..)
    , isCar
    , areCars
    , BinaryTree(..)
    , insertTree
    ) where


newtype Price = Price Int deriving (Show, Eq)

data Vehicle = Car Price | Plane Price deriving (Show, Eq)
data Car = Punto | Focus deriving (Show, Eq)
data Plane = Boeing Int | Airbus Int deriving (Show, Eq)

isCar :: Vehicle -> Bool
isCar (Car _) = True
isCar _  = False


areCars :: [Vehicle] -> Bool
areCars = all isCar --foldr (\x acc -> isCar x && acc) True

data Sum a b = LeftData a | RightData b deriving (Show, Eq)
data Twitter = Twitter deriving (Show, Eq)
data Facebook = Facebook deriving (Show, Eq)
socialNetwork :: Sum Twitter b
socialNetwork = LeftData Twitter

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Show, Eq)

insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf = Node Leaf x Leaf
insertTree x (Node left a right)
  | x < a   = Node (insertTree x left) a right
  | x > a   = Node left a (insertTree x right)
  | otherwise = Node left a right
