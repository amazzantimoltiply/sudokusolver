insert::Ord a=> a ->[a]->[a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

qsort::Ord a=>[a]->[a]
qsort [] =[]
qsort (x:xs)=qsort smaller ++ [x] ++ qsort larger where
    smaller = [a|a<-xs,a<=x]
    larger =[b|b<-xs,b>=x]

fact::Int->Int
fact 0 = 1
fact n | n > 0 =n * fact (n-1)
       | otherwise = 0

sumdown::Int -> Int
sumdown n | n > 0 = n + sumdown (n-1)
          | otherwise = 0
elev::Int->Int->Int
elev b 0 = 1
elev b 1 = b
elev b e | b>0 = b * elev b (e-1)
         | otherwise = b

euclid::Int -> Int -> Int
euclid v v' | v < v' = euclid v (v'- v)
            | v > v' = euclid (v - v') v'
            | otherwise = v

and'::[Bool]->Bool
and' [] = True
and' (x:xs) = x && and' xs

concat'::[[a]] -> [a]
concat' []=[]
concat' (x:xs)= x ++ concat' xs

repl::Int ->a ->[a]
repl 0 _= []
repl n v = v : repl (n-1) v

findnth::[Int] -> Int -> Int
findnth (x:_) 0 = x
findnth [] _ = -1
findnth (_:xs) n = findnth xs (n-1)

element::Eq a => a -> [a] -> Bool
element _ [] = False
element v (x:xs) | x==v = True
              | otherwise = element v xs

sorted:: Ord a => [a] -> Bool
sorted [] = True
sorted (_ :[])=True
sorted (x:y:xs) | x > y = False
              | otherwise = sorted xs

merge::Ord a=>[a]->[a]->[a]
merge xs []= xs
merge [] ys = ys
merge fullX@(x1:x1s) fullY@(x2:x2s) | x1 <= x2 = x1 : merge x1s fullY
                                    | x1 > x2 =  x2 : merge fullX x2s
                        
                              
                              
                        

