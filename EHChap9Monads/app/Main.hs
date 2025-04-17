{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}


module Main (main) where

import Lib ( someFunc )
import Data.String

data Option a = None | Some a deriving Show

data SortedList a = Void | Cons a (SortedList a) deriving stock (Eq,Ord,Show)

insertSorted::Ord a=>a->SortedList a->SortedList a
insertSorted a Void = Cons a Void
insertSorted a (Cons b bs)
    | a >= b = Cons b ( insertSorted a bs)
    |otherwise = Cons a (Cons b bs)
instance Functor SortedList where
    fmap _ Void = Void
    fmap f (Cons a as) =  Cons (f a) (fmap f as)

instance Functor Option where
    fmap _ None = None
    fmap f (Some a) = Some (f a)

data List a = Empty | List a (List a)

concatList::List a ->List a-> List a
concatList Empty as = as
concatList (List a as) bs= List a (concatList as bs)

toList::[a]->List a
toList = foldr List Empty

fromList::List a ->[a]
fromList Empty = []
fromList (List a as) = a : fromList as

instance Show a => Show (List a) where
    show = show . fromList

instance IsString (List Char) where
    fromString = toList

type StringL = List Char

replicateL::Int-> a -> List a
replicateL 0 _ = Empty
replicateL n a = List a (replicateL (n-1) a)

wordsL::StringL->List StringL
wordsL = toList . map toList . words . fromList

instance Functor List where
    fmap _ Empty = Empty
    fmap f (List a as) = List ( f a ) (fmap f as)

instance Applicative List where
    pure a = List a Empty
    Empty <*> _ = Empty
    List f fs <*> vals = concatList (f <$> vals) (fs <*> vals)

instance Monad List where
    return a = List a Empty
    Empty >>= f = Empty
    List a as >>= f = concatList (f a) (as >>= f)


main :: IO ()
main = do
    --print ((\a->(a,succ a)) <$> Some 7)
    --print (fromList (toList [1,2,3]))
    --print (succ <$> List 1 (List 2 (List 3 Empty)) )
    --print (concatList (List 1 (List 2 (List 3 Empty))) (List 4 (List 5 Empty)))
    --print([(+1),(*2),id] <*> [1,2,3])
    --print (toList ['a','b','c'])
    --print ("Hello Haskell"::List Char)
    --print (wordsL "Hello Haskell")
    print (fmap succ (insertSorted 0 (Cons 1 (Cons 2 Void))))
    >> print (fmap odd (insertSorted 0 (Cons 1 (Cons 2 Void)))) 


