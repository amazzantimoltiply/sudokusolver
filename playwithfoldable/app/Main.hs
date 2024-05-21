{-# LANGUAGE DeriveFoldable #-}
module Main (main) where
import Data.Foldable

data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving Foldable

treeToList::Tree a -> [a]
treeToList (Leaf x) = [x]
treeToList (Fork l r) = treeToList l ++ treeToList r

tree= Fork (Leaf 2) (Leaf 3)
main :: IO ()
--main = do print (sum tree)
main = do print (map (+) (Fork (Leaf 2) (Leaf 3)))
