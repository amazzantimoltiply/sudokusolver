{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( Tree(..)
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
    , Show(..)
    , Functor(..)
    , Applicative(..)
    , Monad(..) 
    ) where

import Data.String

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq,Ord,Show)



relable::Tree a -> Int -> (Tree Int,Int)
relable (Leaf _) n = (Leaf n,n+1)
relable (Node l r) n =
    let (l', n')  = relable l n
        (r', n'') = relable r n'
    in (Node l' r', n'')    
    

data Option a = None | Some a deriving Show

type State = Int
newtype ST a = S (State -> (a, State))

app:: ST a -> State -> (a, State)
app (S f) st = f st

instance Functor ST where
    fmap g (S f) = S (\s -> let (a, s') = f s in (g a, s'))
instance Applicative ST where
    pure a = S (\s -> (a, s))
    S f <*> S g = S (\s -> let (h, s') = f s
                               (x, s'') = g s'
                           in (h x, s''))
instance Show (ST a) where
    show _ = "<ST function>"

instance Monad ST where
    st >>= f = S (\s -> let (x, s') = app st s
                            in app (f x) s')


data SortedList a = Void | Cons a (SortedList a) deriving (Eq,Ord,Show)

insertSorted::Ord a=>a->SortedList a->SortedList a
insertSorted a Void = Cons a Void
insertSorted a (Cons b bs)
    | a >= b = Cons b ( insertSorted a bs)
    |otherwise = Cons a (Cons b bs)
instance Functor SortedList where
    fmap _ Void = Void
    fmap f (Cons a as) =  Cons (f a) (fmap f as)

instance Applicative SortedList where
    pure a = Cons a Void
    Void <*> _ = Void
    Cons f fs <*> vals = concatSortedLists (f <$> vals) (fs <*> vals)
instance Monad SortedList where
    return a = Cons a Void
    Cons a as >>= f = concatSortedLists (f a) (as >>= f)

instance Functor Option where
    fmap _ None = None
    fmap f (Some a) = Some (f a)
instance Applicative Option where
    pure a = Some a
    None <*> _ = None
    (Some f) <*> a = fmap f a

concatSortedLists::SortedList a -> SortedList a -> SortedList a
concatSortedLists Void as = as
concatSortedLists (Cons a as) bs = Cons a (concatSortedLists as bs)

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

