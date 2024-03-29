{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
twice::(a->a)->a->a
twice f x = f (f x)

map'::(a->b) -> [a] -> [b]
map' f xs =[f x | x <- xs]

map''::(a->b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

sum'::Num a =>[a]->a
sum' = foldr (+) 0

map3::(Num a,Num b) =>(a->b)->(a->Bool)->[a]->[b]
map3 f p xs =[f x | x <- xs, p x]

map4::(Num a,Num b) =>(a->b)->(a->Bool)->[a]->[b]
map4 f p xs = map f (filter p xs)

map5::(Num a,Num b) =>(a->b)->(a->Bool)->[a]->[b]
map5 _ _ []=[]
map5 f p (x:xs) | p x  = f x : map5 f p xs
                | otherwise = map5 f p xs

all_new::(a -> Bool) -> [a] -> Bool
all_new _ []=True
all_new f (x:xs)=  f x && all_new f xs