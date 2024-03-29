import Data.Char

cls::IO()
cls = putStr "\ESC[2J"

type Pos=(Int,Int)

writeat::Pos->String->IO()
writeat p xs = do goto p
                  putStr xs
goto::Pos->IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width::Int
width = 5
height::Int
height = 5

type Board = [Pos]
glider::Board
glider = [(4,3),(2,3),(4,3),(3,4),(4,4)]

showcells::Board->IO()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive::Board->Pos->Bool
isAlive b p = elem p b

isEmpty::Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs::Pos->[Pos]
neighbs (x,y)=map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

wrap::Pos->Pos
wrap (x,y) = (mod (x-1) width +1,mod (y-1) height+1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors::Board->[Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births::Board->[Pos]
births b =[(x,y)| x <- [1..width],y <-[1..height],isEmpty b (x,y),liveneighbs b (x,y) == 3]

nextgen::Board->Board
nextgen b = survivors b ++ births b

wait::Int -> IO()
wait n = sequence_[return() | _<-[1..n]]

myPutStr::String->IO()
myPutStr s = do 
               sequence_[putChar c|c <-s]
               putChar '\n'
newline::IO()
newline = putChar '\n'

decval::Int->Int
decval v = v - 1

getandsumnumbers::Int->Int->IO Int
getandsumnumbers val 0 = return val
getandsumnumbers s left = do putStr "Insert num:"
                             x <- getChar
                             newline
                             if isDigit x then
                              do
                                let temp_left = left
                                let temp_sum = s
                                let left = temp_left - 1
                                let s = digitToInt x + temp_sum
                                getandsumnumbers s left
                              else
                                do 
                                  putStr "Not a number"
                                  putChar '\n'
                                  getandsumnumbers s left



life::Board -> IO()
life b = do cls
            myPutStr "Game of life"
            showcells b
            wait 500000
            life (nextgen b)