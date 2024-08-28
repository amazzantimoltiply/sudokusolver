{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main (main) where


data Tree = Nil | Node Tree Int Tree  deriving Show

insert::Int -> Tree -> Tree
insert v Nil = Node Nil v Nil
insert v (Node l v1 r)
    | v < v1 = Node (insert v l) v1 r
    | v > v1 = Node l v1 (insert v r)
    | otherwise = Node l v1 r

sample_tree::Tree
sample_tree = Node Nil 10 Nil

treefromlist::[Int]->Tree
treefromlist = foldr insert Nil

listfromtree::Tree->[Int]
listfromtree Nil = []
listfromtree (Node l v r) = listfromtree l ++ [v] ++ listfromtree r

searchelem::Int->Tree->Bool
searchelem _ Nil = False
searchelem v (Node l v1 r )
  | v == v1 = True
  | v < v1 = searchelem v l
  | v > v1 = searchelem v r

depth::Tree->Int
depth Nil = 0
depth (Node l _ r) = 1 + max (depth l) (depth r) 

nodeatDepth::Int->Tree->[Int]
nodeatDepth v Nil = []
nodeatDepth 0 (Node l v r) = [v]
nodeatDepth n (Node l v r) = nodeatDepth (n-1) l ++ nodeatDepth (n-1) r   

dfttraverse::Tree->[Int]
dfttraverse Nil = []
dfttraverse (Node l v r) = v : dfttraverse l ++ dfttraverse r


t :: Tree
t = Node (Node (Node Nil 4 Nil) 2 (Node Nil 5 Nil)) 1 (Node (Node (Node Nil 8 Nil) 6 (Node Nil 9 Nil)) 3 (Node Nil 7 Nil))

main :: IO ()
main = do
    --print (insert 1 (insert 12 (insert 6 (insert 4 sample_tree))))
    --print (listfromtree (treefromlist [5,6,7,2,3,4]))
    --print (searchelem 13 (treefromlist [2,7,6,8,9,10,12]))  
    --print (nodeatDepth 3 (treefromlist [10,4,7,2,9,12,1,3,5]))
    print (dfttraverse t)
