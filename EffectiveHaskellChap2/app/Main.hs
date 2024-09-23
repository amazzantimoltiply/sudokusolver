module Main (main) where

import Lib

foodCosts = [("Andrea",10.00),("Stefano",15.00),("Luca",5.00)]

data Expr = X | Lit Int | Add Expr Expr | Mul Expr Expr

eval::Expr -> Int -> Int
eval X x = x
eval (Lit n) _ = n
eval (Add ex1 ex2) x = eval ex1 x + eval ex2 x
eval (Mul ex1 ex2) x = eval ex1 x + eval ex2 x

foldExpr::(b->b->b)->(Int->b)->Expr->b->b
foldExpr fa fb X x = x
foldExpr fa fb (Lit n) _= fb n
foldExpr fa fb (Add ex1 ex2) x = fa (foldExpr fa fb ex1 x) (foldExpr fa fb ex2 x)

renderExpr::Expr -> String
renderExpr X = "x"
renderExpr (Lit n) = show n
renderExpr (Add ex1 ex2) =
    parens (renderExpr ex1 ++ "+" ++ renderExpr ex2)
renderExpr (Mul ex1 ex2) = renderExpr ex1 ++ "*" ++ renderExpr ex2

parens::String->String
parens s = "(" ++ s ++ ")"

type Prec = Int
basePrec,addPrec,mulPrec::Prec
basePrec=0
addPrec =6
mulPrec =7

renderExpr'::Expr->String
renderExpr' e = go e basePrec where
    go::Expr->Prec->String
    go X p           = "x"
    go (Lit n ) p    = show n
    go (Add e1 e2) p =
        parensP p addPrec (go e1 addPrec ++ "+" ++ go e2 addPrec)
    go (Mul e1 e2) p =
        go e1 mulPrec ++ "*" ++ go e2 mulPrec
    parensP::Prec->Prec->String->String
    parensP p1 p2 s
        |p1 <= p2 = s
        |otherwise = parens s

tails::[a]->[[a]]
tails [] = []
tails (x:xs) = xs:tails xs


findThatIsTrue fToTestIt = foldr findHelper []
    where
        findHelper val rest
            | fToTestIt val = [val]
            | otherwise = rest

main :: IO ()
main =
    --print (factors 8)
    --print (isBalanced "(()")
    --print (doubleElems [1,2,3])
    --print (partyBudget (checkGuestList["Andrea","Stefano"]) foodCosts) 
    --print (pairs [1..10][2..5])
    --print (pairWiseSum [1,2,3][1,2,3])
    --print (checkList [0,1,2,3,4])
    --print (evenLenght [1,2,3,4])
    --print (eval (Add (Lit 2) (Lit 3)))
    --print (renderExpr'(Add (Mul (Lit 2) (Lit 3)) X ))
    --print (foldExpr (+) id (Add (Add (Lit 3) (Lit 3)) X) 3 )
    --print (tails [1,2,4,5,6,6,7,54,5,63])
    print (findThatIsTrue (>5) [1..6])
