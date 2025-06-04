{-# LANGUAGE NoImplicitPrelude #-}

import Lib (Tree(..), relable)
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
import Prelude (Char, Bool(..), ($), (<), (==), (++), putStrLn, and, tail, IO, div, (+), zipWith)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized arbitraryTree
    where
      arbitraryTree 0 = Leaf <$> arbitrary
      arbitraryTree n = oneof [
        Leaf <$> arbitrary,
        Node <$> subtree <*> subtree
        ]
        where subtree = arbitraryTree (n `div` 2)

-- Test that relabeling preserves structure
prop_relabelStructure :: Tree Char -> Bool
prop_relabelStructure tree =
  let (relabeled, _) = relable tree 0
      countNodes (Leaf _) = 1
      countNodes (Node l r) = countNodes l + countNodes r
  in countNodes tree == countNodes relabeled

-- Test that labels are strictly increasing
prop_relabelIncreasing :: Tree Char -> Bool
prop_relabelIncreasing tree =
  let (relabeled, _) = relable tree 0
      getLabels (Leaf n) = [n]
      getLabels (Node l r) = getLabels l ++ getLabels r
      labels = getLabels relabeled
  in and $ zipWith (<) labels (tail labels)

main :: IO ()
main = do
  putStrLn "Testing structure preservation..."
  quickCheck prop_relabelStructure
  putStrLn "Testing increasing labels..."
  quickCheck prop_relabelIncreasing
