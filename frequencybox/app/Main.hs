module Main (main) where

import Lib

data Box = MkBox {content::[String],width::Int,height::Int}
examplebox::Box
examplebox = MkBox ["abcd","efgh","ijkl"] 4 3
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
main :: IO ()
main = do
    print (frame (vboxcompose examplebox examplebox))
