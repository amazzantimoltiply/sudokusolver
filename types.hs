{-# LANGUAGE DataKinds #-}
type Pos = (Int,Int)
data Mov = North | South | East | West deriving Show

type Assoc k v = [(k,v)]

fnd :: Eq k => k -> Assoc k v -> v
fnd k t = head [v | (k', v) <- t , k'== k]

move::Mov -> Pos -> Pos
move North (x,y)=(x,y+1)
move South (x,y)=(x,y-1)
move East (x,y)=(x+1,y)
move West (x,y)=(x-1,y)

moves::[Mov]->Pos->Pos
moves ms p = foldl (flip move) p ms

{-
moves [] p = p
moves (m:ms) p = moves ms (move m p)
-}

data Shape = Circle Float | Rect Int Int deriving Show
square::Int -> Shape
square n = Rect n n 

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show
t::Tree Int
t=Node (Node (Leaf 1) 3 (Leaf 2)) 7 (Node (Leaf 4) 6 (Leaf 5))
t1=Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 10))

sumtree:: Num a => Tree a -> a
sumtree (Leaf v) = v
sumtree (Node l y r) = y + sumtree l + sumtree r



balanced::(Ord a,Num a) => Tree a -> Bool
balanced t = countleft t <= countrigth t
    where
        countleft::Num a => Tree a -> a
        countleft (Leaf v)=1
        countleft (Node l _ _) = 1 + countleft l  

        countrigth::Num a => Tree a -> a
        countrigth (Leaf v)=1
        countrigth (Node _ _ r) = 1 + countrigth r  



flattent::Tree a -> [a]
flattent (Leaf v) = [v]
flattent (Node l v r) = flattent l ++ [v] ++ flattent r 

findvalue::Ord a=> a->Tree a->Bool
findvalue x (Leaf v) = x == v
findvalue x (Node l v r) | x == v = True
                         | x < v = findvalue x l 
                         | otherwise = findvalue x r
                         
list2tree::Num a => [a] -> Tree a              
list2tree [x] = Leaf x
list2tree list = Node (list2tree ltx) x (list2tree gtx)
                where 
                    m = length list `div` 2
                    x = list !! m
                    ltx = take m list
                    gtx = drop (m+1) list
            
                         
data Expr = Val Int | Add Expr Expr
value::Expr -> Int 
value (Val v) = v
value (Add x y) = value x + value y
v::Expr
v = Add (Add(Val 2)(Val 3)) (Add(Val 3) (Val 2))

data Nat = Zero | Succ Nat deriving Show
nat2int::Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat::Int->Nat
int2nat 0 =Zero
int2nat n = Succ(int2nat(n-1))

addnat::Nat->Nat->Nat
addnat n m = int2nat (nat2int n + nat2int m ) 

mulnat::Nat->Nat->Nat
mulnat n m = int2nat (nat2int n * nat2int m)

test::Nat
test=Succ(Succ(Succ Zero))