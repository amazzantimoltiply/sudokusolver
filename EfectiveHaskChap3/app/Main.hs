{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}



module Main (main) where
import Text.Read (readEither)

data StringParser = StringParser{
    runStringParser::String->(String,String)
}




takeChars numChars = StringParser $ \inputstring -> splitAt numChars inputstring

data CustomerInfo = CustomerInfo {
    firstName::String,
    lastName::String,
    deposit::Int,
    balance::Int
} deriving Show


data Person = Customer {
    name::String,
    customerBalance::String
} | Employee {
    name::String,
    id::Int,
    contact:: PreferredCustomerContact
}

data Expr = Lit Int
        | Sub Expr Expr
        | Add Expr Expr
        | Mul Expr Expr
        | Div Expr Expr deriving Show

eval::Expr -> Int
eval expr = case expr of
        Lit num -> num
        Add arg1 arg2 -> eval' (+) arg1 arg2
        Sub arg1 arg2 -> eval' (-) arg1 arg2
        Mul arg1 arg2 -> eval' (*) arg1 arg2
        Div arg1 arg2 -> eval' div arg1 arg2
        where
            eval'::(Int->Int->Int) -> Expr ->Expr -> Int
            eval' opr arg1 arg2 = opr (eval arg1) (eval arg2)



data Peano = Z | S Peano deriving Show

toPeano::Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n-1)

fromPeano::Peano -> Int
fromPeano Z = 0
fromPeano (S p) = 1 + fromPeano p

class Natural n where
    equal::n->n->Bool
    add::n->n->n
    mul::n->n->n
    addId::n
    mulId::n 
    

instance Natural Int where
    add = (+)
    mul = (*)
    addId = 0
    mulId = 1

instance Eq Peano where
    (==) :: Peano -> Peano -> Bool
    (==) Z Z = True
    (==) (S a) (S b) = a==b
    (==) _ _ = False
    

instance  Natural Peano where
    add :: Peano -> Peano -> Peano
    add a Z = a
    add a (S b) = add (S a) b
    mul _ Z = Z
    mul Z _ = Z
    mul (S a) b = add b (mul a b)
    addId = Z
    mulId = S Z
     


data PreferredCustomerContact = Email String | TextMessage String | Phone String

getPreferredCustomerContact:: PreferredCustomerContact -> String
getPreferredCustomerContact contact =
    case contact of
        Email mail -> "Preferred contact is : Email, current mail is: " <> mail
        TextMessage text -> "Prefferred contact is: Text Message, current text is: " <> text
        Phone phone-> "Preferred contact is : phone, current phone num. is: " <> phone


buildCustomerList::[CustomerInfo]->CustomerInfo->[CustomerInfo]
buildCustomerList [] ci = [ci]
buildCustomerList cis cinew = cinew : cis

{-
parse :: String -> Either String Expr
parse str =
    case parse' (words str) of
        Left err -> Left err
        Right (e,[]) -> Right e
        Right (_,rest) -> Left $ "Found extra tokens:" <> unwords rest
-}

{-
parse' [] = Left "Unexpected end of expresion"
parse' (token:rest) =
    case token of
        "+" -> parseBinary Add rest
        "-" -> parseBinary Sub rest
        "*" -> parseBinary Mul rest
        "/" -> parseBinary Div rest
        --lit -> case readEither lit of
        --    Left err -> Left err
        --    Right lit' -> Right (lit',rest)
-}

{- parseBinary::(Expr->Expr->Expr)->[String]->Either String (Expr,[String])
parseBinary oper args =
    case parse' args of
        Left err -> Left err
        Right (arg1,rest) ->
            case parse' rest of
                Left err -> Left err
                Right (arg2, rest') ->
                    Right $ (oper arg1 arg2, rest')
-}

data Tree a = EmptyTree | Node a (Tree a) (Tree a)

insertNode::(Ord a) => a -> Tree a  -> Tree a
insertNode x EmptyTree = Node x EmptyTree EmptyTree
insertNode x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (insertNode x left) right
    | x > a = Node a left (insertNode x right)

instance (Show a ) => Show (Tree a) where
    show EmptyTree = ""
    show (Node a l r) = show a <> show l <> show r

main = do
    --print (firstName CustomerInfo {firstName="Andrea",lastName="Mazzanti",deposit=0,balance=0})
    --print (getPreferredCustomerContact (Email "andreama@microsoft.com"))
    --let andrea = Employee {name="Andrea",id=1,contact=TextMessage "+39 3440560238"}
    --print (name andrea)
    --print (getPreferredCustomerContact $ contact andrea)
    --print (fromPeano (toPeano 3))
    --print (eval $ Div (Add (Lit 1) (Lit 3)) (Lit 2) )
    --print (runStringParser (takeChars 3) "Andrea" )
    --print (insertNode 2 (insertNode 3 (Node 0 EmptyTree EmptyTree)))
    print (mul (S (S Z)) (S (S Z)) ) 
