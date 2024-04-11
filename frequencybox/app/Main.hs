module Main (main) where

import Lib

data Box = MkBox {content::[String],width::Int,height::Int}
examplebox::Box
examplebox = MkBox ["abcd","efgh","ijkl"] 4 3

emptyBox::Box
emptyBox = MkBox [] 0 0

instance Show Box where
    show = unlines . content
frame::Box->Box
frame (MkBox c w h)=
    MkBox ([tline] ++ map vline c ++ [bline]) (w + length c) (h+2) where
        tline =  "   " ++ replicate w  '_' 
        bline =  "   " ++ replicate w  '_'
        vline l = " | " ++ l ++ " | " 
vboxcompose::Box -> Box -> Box
vboxcompose (MkBox c1 w1 h1) (MkBox c2 w2 h2)=
    MkBox (c1' ++ c2') w h
        where
            w = max w1 w2
            h = h1 + h2
            pad n l = l ++ replicate n ' '
            c1' = map (pad (w-w1)) c1
            c2' = map (pad (w-w2)) c2

hboxcompose::String->Box->Box->Box
hboxcompose sep (MkBox c1 w1 h1) (MkBox c2 w2 h2) =
    MkBox (zipWith (\l1 l2 -> l1 ++ sep ++ l2) c1' c2') w h where
        w = w1 + length sep + w2
        h = max h1 h2
        pad n m l = l ++ replicate n (replicate m ' ')
        c1' = pad (h-h1) w1 c1
        c2' = pad (h-h2) w2 c2
listofBox::[Box]
listofBox = [MkBox ["abcd","efgh","ijkl"] 4 3,MkBox ["abcd","efgh","ijkl"] 4 3]
vboxCompList::[Box] -> Box
vboxCompList = foldr vboxcompose emptyBox

main :: IO ()
main = do
    --print (frame (vboxcompose examplebox examplebox))
    --print (vboxCompList listofBox)
    print (frame (hboxcompose "<->" examplebox examplebox))
