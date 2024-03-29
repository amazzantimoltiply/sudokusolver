module TestHask where
    data Employee = Coder | Manager | Veep | CEO deriving (Ord,Eq,Show)
    reportBoss::Employee -> Employee -> IO()
    reportBoss e e' = 
        putStrLn $ show e ++ " is the boss of " ++ show e'
    hackingCoder::Employee -> Employee -> Ordering
    hackingCoder Coder Coder = EQ
    hackingCoder _ Coder = GT
    hackingCoder Coder _ = LT
    hackingCoder e e' = compare e e'
    
    employeeRank:: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO()
    employeeRank f e e'=
        case f e e' of
           GT -> reportBoss e e'
           EQ -> putStrLn "Neither employee is the boss "
           LT -> (flip reportBoss) e e'

    isIpotenuse::(Num a ,Eq a) => a -> a -> a -> String
    isIpotenuse a b c 
        | a^2 + b^2 == c = "It is right"
        | otherwise = "Not right"
    avgGrade:: (Fractional a, Ord a ) => a -> Char
    avgGrade x
        | y > 0.8 = 'B'
        | y > 0.7 = 'C'
        | y >= 0.59 = 'D'
        | y < 0.59 = 'F'
        | y > 0.9 = 'A'
        where y = x / 100



