{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

module Main (main) where

import Data.List ( sort )
import Data.Kind

import Prelude hiding (null)


rmdups::(Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x: filter (/=x) (rmdups xs)

rmdupsSort::(Ord a) => [a] -> [a]
rmdupsSort = rmd . sort where
    rmd (c:cs)
        | cs == [] = [c]
        | c == head cs = rmd cs
        | otherwise = c : rmd cs

class Redacted a where
    redacted:: a -> String
    printclear::a ->String

data UserName = UserName String

instance Show UserName where
    show :: UserName -> String
    show (UserName un) = showhided un where
        showhided :: String -> String
        showhided (_:cs) = "*" <> showhided cs
        showhided [] = "*"


instance Redacted UserName where
    redacted :: UserName -> String
    redacted = show
    printclear (UserName name) = name


toCSV::(Foldable t, Show a) =>t a->String
toCSV =
    let addField::Show a => String -> a ->String
        addField s a = s <> "," <> show a
        dropLeadingComma::String->String
        dropLeadingComma s =
            case s of
                ',':s'->s'
                _ -> s
    in dropLeadingComma . foldl addField ""

newtype Sel (f::Type->Type) (a::Type) = Sel (f a) deriving Show
instance (Select f) => Semigroup (Sel f a) where
    (Sel a) <> (Sel b ) = Sel (pick a b)

instance (Select f) => Monoid (Sel f a) where
    mempty = Sel empty

class Select (f::Type->Type) where
    empty::f a
    pick::f a -> f a -> f a
instance Select Maybe where
    empty = Nothing
    pick Nothing a = a
    pick a _ = a
instance Select [] where
    empty = []
    pick = (<>)
newtype MyMaybe a = MyMaybe (Maybe a) deriving Show

instance Semigroup (MyMaybe a) where
    (MyMaybe a) <> (MyMaybe b) = MyMaybe (pick a b)

instance Monoid (MyMaybe a) where
    mempty = MyMaybe empty

class Nullable a where
    isNull::a->Bool
    null::a

instance Nullable (Maybe a) where
    isNull Nothing = True
    isNull _ = False
    null = Nothing
instance Nullable [a] where
    isNull [] = True
    isNull _ = False
    null = []
instance (Nullable a,Nullable b) => Nullable (a,b) where
    isNull (a,b) = isNull a && isNull b
    null = (null,null)


main :: IO ()
main = do
    --print (rmdups [1,2,1,5,6,5])
    --print (redacted (UserName "Andrea"))
    --print (rmdups (printclear (UserName "Andrea Mazzanti")))
    --print (toCSV @Maybe @String Nothing)
    --print (MyMaybe (Just 1) <> MyMaybe (Just 2))
    --print (Sel [1,2,3] <> Sel [1,2])
    --print (Sel (Just 1) <> Sel (Just 2))
    --print (pick [] [1,2,3])
    --print (foldl pick [Nothing, Just 1 , Nothing] Nothing)
    print (isNull (Nothing,Just 1))
    

