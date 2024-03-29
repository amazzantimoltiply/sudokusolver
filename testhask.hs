
module TestAssertion where

    --triple :: Integer -> Integer
    --triple x = doTriple x
      --  where doTriple::Integer -> Integer
        --      doTriple x = x * 3

    --isPalindrome :: (Eq a) => [a] -> Bool
    --isPalindrome x = reverse x == x

    --myAbs::Integer -> Integer
    --myAbs x = if x > 0 then x * (-1) else x
    --f :: (a, b) -> (c, d) -> ((b, d), (a, c))
    --f x y = ((snd x,snd y),(fst x,fst y))
    --x = (+)
    --f::[Char] -> Int
    --f xs = x lenplusone 1
    --   where lenplusone=length xs
    data TisAnInteger where
      TisAn :: Integer -> TisAnInteger  deriving Show

    instance Eq TisAnInteger where
      (==) (TisAn x) (TisAn y) =  x == y

    data TwoInteger where
      Two :: Integer -> Integer -> TwoInteger

    instance Eq TwoInteger where
      (==) (Two x y) (Two x' y') =  x==x' && y==y'

    data DayOfWeek where
      Mon :: DayOfWeek
      Tue :: DayOfWeek
      Wed :: DayOfWeek
      Thu :: DayOfWeek
      Fri :: DayOfWeek
      Sat :: DayOfWeek
      Sun :: DayOfWeek deriving Show
    instance Ord DayOfWeek where
        compare Fri Fri = EQ
        compare _ Fri = LT
        compare Fri _ =  GT
        compare _ _ = EQ

    data Date where
      Date :: DayOfWeek -> Int -> Date  deriving Show

    instance Eq DayOfWeek where
      (==) Mon Mon =  True
      (==) Tue Tue =  True
      (==) Wed Wed =  True
      (==) Thu Thu =  True
      (==) Fri Fri =  True
      (==) Sat Sat =  True
      (==) Sun Sun =  True
      (==) _ _ =  False

    instance Eq Date where
      (==) (Date weekday dayOfMonth)(Date weekday' dayOfMonth') =
         weekday == weekday' && dayOfMonth == dayOfMonth'
    main= do
        Date Tue 10 == Date Mon 10

