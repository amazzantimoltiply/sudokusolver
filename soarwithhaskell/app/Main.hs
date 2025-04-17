module Main (main) where

import Lib

data Product = MkP {product::String,price::Int} deriving Show

scaffold=[MkP "medicine" 8, MkP "rice" 1, MkP "sweets" 3]

data Expr = Var String | Lit Int | Add Expr Expr --deriving Eq
expr::Expr
expr = Add (Var "x") (Add (Lit 3) (Var "y")) -- x + (3 + y)

type Env = [(String,Int)]

list::Env
list=[("x",10),("y",11)]

eval::Expr->Env->Int 
eval (Var s) env = lookupenv env s 
eval (Lit n) env = n
eval (Add e1 e2) env = eval e1 env + eval e2 env

lookupenv::Env->String->Int
lookupenv [] _ =0
lookupenv ((s,v):env) w
    | s==w = v
    | otherwise = lookupenv env w

instance Eq Expr where
    Var s1 == Var s2 = Eval (Var s1 ) == Eval (Var s2)
    (Lit n == Lit m) = n == m
    (Add e1 e2 == Add e3 e4) = (e1 == e3) && (e2 == e4) 
    _ == _ = False


main :: IO ()
main = 
    --print (dropWhiteSpaces "  c  andrea  mazz  go ")
    --print (insertGen (>=) [1,3,4,5] 6)
    --print (iSortGen (<) [3,6,7,2,3,0,9])
    --print (summWithFold [1,2,3,4,5])
    print (myScanR (-) 0 [0,1,2,3,4,5])
    --print (average [1,2,3,6,10,6,8,3,5]) 
    --print (zip scaffold (tails' (scanl (+) 0 (map price scaffold ))))
    --print (map price [MkP "medicine" 8, MkP "rice" 1, MkP "sweets" 3])
    --print (eval expr list)
    --print (Lit 3 == Add (Lit 1) (Lit 2) )
    --print (deleteSame [1,2,3,1,4,5,6,1,3] 1 )

