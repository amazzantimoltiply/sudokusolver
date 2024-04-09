module Main (main) where

import Lib

data Box = MkBox {content::[String],width::Int,height::Int}
examplebox::Box
examplebox = MkBox ["abcd","efgh","ijkl"] 4 3
instance Show Box where
    show = unlines . content
frame::Box->Box
frame (MkBox c w h)=
    MkBox ([tline] ++ map vline c ++ [bline]) (w+2) (h+2) where
        tline = " [" ++ replicate w '_' ++ "] " 
        bline = " [" ++ replicate w '_' ++ "] "
        vline l = " | " ++ l ++ " | " 
main :: IO ()
main = do
    print (frame examplebox)
