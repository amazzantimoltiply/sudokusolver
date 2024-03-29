import Data.Char

let2int::Char -> Int
let2int c = ord c - ord 'a'

int2let::Int -> Char
int2let n = chr (ord 'a' + n)

shift::Int->Char->Char
shift n c | isLower c = int2let (mod (let2int c + n) 26)
          | otherwise = c
encode::Int->String->String
encode n xs= [shift n x | x <- xs]

percent::Int->Int->Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

lowers :: String -> Int
lowers xs = length (filter isLower xs)

freqs::String->[Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
        where n = lowers xs

grid::Int->Int->[(Int,Int)]
grid x y= [(x',y') | x' <- [0..x],y' <-[0..y]]

square::Int->[(Int,Int)]
square x = [(x',y') | x' <- [0..x],y' <-[0..x],x'/=y']

myreplicate n v = [ v | _ <- [1..n]]

pyth::Int->[(Int,Int,Int)]
pyth n =  [(x,y,z) | x<-[1..n],y<-[1..n],z<-[1..n],x^2+y^2==z^2]

factors::Int -> [Int]
factors n = [x | x <- [1..n], mod n x ==0]

perfect::Int->Bool
perfect n = sum (filter (/=n) (factors n)) == n

perfects n = [x|x<-[1..n],perfect x]

