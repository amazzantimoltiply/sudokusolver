module Main (main) where
import Lib

data Segno = Quandri | Picche | Fiori | Cuori deriving Show
data Tipo = Numeral Int | Facce Simboli deriving Show   
data Simboli = Jack | Regina | Re deriving Show
data Carta = MkCarta Segno Tipo | Joker deriving Show

main :: IO ()
main = do
    --let andrea = MkPerson "Andrea" 54
    --print (age andrea)
    let aceofspades = MkCarta Picche (Numeral 1)
    print aceofspades


