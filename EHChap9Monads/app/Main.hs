{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}  

module Main (main) where

-- Import from our library
import Lib (Tree(..)
         , relable
         , Option(..)
         , State
         , ST(..)
         , SortedList(..)
         , insertSorted
         , concatSortedLists
         , List(..)
         , concatList
         , toList
         , fromList
         , StringL
         , replicateL
         , wordsL
         , app
         , IsString(..)
         )

-- Import from Prelude
import Prelude (print
              , putStrLn
              , Int
              , Integer
              , (+)
              , (-)
              , (*)
              , (.)
              , ($)
              , Show(..)
              , String
              , Char
              , IO
              , fmap
              , pure
              , (<*>)
              , (<$>)
              , (>>=)
              , (>>)
              , return
              , succ
              , id
              , Num(..)
              , Enum(..)
              , Ord(..)
              )

-- Test Option type
testOption :: IO ()
testOption = print (pure (+) <*> Some 3 <*> None)

-- Test List operations
testList :: IO ()
testList = do
    print (fromList (toList [1,2,3]))
    print (concatList (List 1 (List 2 (List 3 Empty))) (List 4 (List 5 Empty)))
    print (toList ['a','b','c'])
    print ("Hello Haskell"::List Char)
    print (wordsL "Hello Haskell")

-- Test SortedList operations
testSortedList :: IO ()
testSortedList = do
    print (concatSortedLists (fmap succ (insertSorted 0 (Cons 1 (Cons 2 Void)))) 
                            (fmap succ (insertSorted 0 (Cons 1 (Cons 2 Void)))))
    print ((-) . (*2) <$> insertSorted 0 (Cons 2 Void) <*> ((+1) <$> Cons 3 Void))
    print ((((+)) <$> Void) <*> Cons 1 Void)

-- Test State operations
testState :: IO ()
testState = print (((+) <$> S (\s -> (1 :: Int, s))) <*> S (\s -> (2 :: Int, s)))

-- Test Tree relabeling
testRelabel :: IO ()
testRelabel = print (relable (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) 1)

-- Main program that runs all tests
main :: IO ()
main = do
    _ <- putStrLn "Testing Option type..."
    _ <- testOption
    _ <- putStrLn "\nTesting List operations..."
    _ <- testList
    _ <- putStrLn "\nTesting SortedList operations..."
    _ <- testSortedList
    _ <- putStrLn "\nTesting State operations..."
    _ <- testState
    _ <- putStrLn "\nTesting Tree relabeling..."
    testRelabel