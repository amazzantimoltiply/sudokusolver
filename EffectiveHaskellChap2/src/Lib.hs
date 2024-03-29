module Lib
    ( factors,isBalanced,doubleElems,partyBudget,checkGuestList,pairs,pairWiseSum,checkList,evenLenght,
    ) where

factors::Int->[Int]
factors num =
    factors' num 2
    where
        factors' num fact
            | num == 1 = []
            | rem num fact == 0 = fact : factors' (div num fact) fact
            | otherwise = factors' num (fact + 1)
isBalanced str =
    0 == reduce checkBalance 0 str
    where
        checkBalance count letter
            | letter == '(' = count + 1
            | letter == ')' = count - 1
            | otherwise = count

reduce func accumulator lst =
    if null lst then accumulator
    else
        let midvalue = func accumulator (head lst)
        in reduce func midvalue (tail lst)


doubleElems elems = foldr doubleElems' [] elems
    where
        doubleElems' num lst = (2 * num):lst

checkGuestList guestList name = name `elem` guestList

partyBudget isAttending =
    foldr (+) 0 . map snd . filter (isAttending.fst)

pairs as bs =
    let as' = filter (elem bs) as
        bs' = filter odd bs
        mkPairs a = map (\b -> (a,b)) bs'
        in concat $ map mkPairs as'

pairWiseSum as bs=
   let sumPair pairs=
        let a = fst pairs
            b = snd pairs
        in a + b

    in map sumPair $ zip as bs

checkList::(Integral a,Show a,Eq a) => [a]->String
checkList l =
    case l of
        [] -> "Is empty lis"
        [x] | x == 0 -> "Is calling:[0]"
            | x == 1 -> "Is a singluar list of [1]"
            | even x -> "A singleton list of an even number"
            | otherwise -> "The lis contains " <> show x
        _list -> "The list has more than one element"

evenLenght::[Int]-> Bool
evenLenght [] = True
evenLenght (x:xs) = not (evenLenght xs)

