{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( Vehicle(..)
    , Car(..)
    , Plane(..)
    , Price(..)
    , isCar
    , areCars
    , BinaryTree(..)
    , insertTree
    , TraversalOrder(..)
    , treeToList
    , Sum(..)
    , Optional(..)
    , Twitter(..)
    , Facebook(..)
    , Exclamation(..)
    , Adverb(..)
    , MySemigroup(..)
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

-- Traversal order options
data TraversalOrder = Preorder | Inorder | Postorder deriving (Show, Eq)

-- Make BinaryTree an instance of Functor for standard mapping
instance Functor BinaryTree where
  fmap _ Leaf = Leaf
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree x Leaf = Node Leaf x Leaf
insertTree x (Node left a right)
  | x < a   = Node (insertTree x left) a right
  | x > a   = Node left a (insertTree x right)
  | otherwise = Node left a right

-- Single traversal function with order parameter
treeToList :: TraversalOrder -> BinaryTree a -> [a]
treeToList _ Leaf = []
treeToList order (Node left a right) = case order of
  Preorder  -> [a] ++ treeToList order left ++ treeToList order right    -- root, left, right
  Inorder   -> treeToList order left ++ [a] ++ treeToList order right    -- left, root, right
  Postorder -> treeToList order left ++ treeToList order right ++ [a]    -- left, right, root

data Optional a = Nada | Only a deriving (Show, Eq)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> y = y
  x <> Nada = x
  Only x <> Only y = Only (x <> y)

instance Semigroup a => Monoid (Optional a) where
  mempty = Nada
 
instance (Semigroup a, Semigroup b) => Semigroup (Sum a b) where
  LeftData x <> LeftData y = LeftData (x <> y)
  LeftData x <> RightData y = RightData y
  RightData x <> LeftData y = RightData x
  RightData x <> RightData y = RightData (x <> y) 

instance (Monoid a, Semigroup b) => Monoid (Sum a b) where
  mempty = LeftData mempty

newtype Exclamation = Exclamation String deriving (Show, Eq)
newtype Adverb = Adverb String deriving (Show, Eq)

instance Semigroup Exclamation where
  Exclamation x <> Exclamation y = Exclamation (x <> y)

instance Monoid Exclamation where
  mempty = Exclamation mempty 

instance Semigroup Adverb where
  Adverb x <> Adverb y 
    | null x = Adverb y
    | null y = Adverb x  
    | otherwise = Adverb (x <> " " <> y)

instance Monoid Adverb where
  mempty = Adverb mempty

class MySemigroup a where
  myAppend :: a -> a -> a 

instance MySemigroup Exclamation where
  myAppend (Exclamation x) (Exclamation y) = Exclamation (x <> y)

instance MySemigroup Adverb where
  myAppend (Adverb x) (Adverb y) 
    | null x = Adverb y
    | null y = Adverb x  
    | otherwise = Adverb (x <> " " <> y) 