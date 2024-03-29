module Chap7 where
    
    add::Int -> Int -> Int
    add x y = x + y 

    addPf::Int -> Int -> Int
    addPf = (+)
    addOne::Int -> Int
    addOne = \x -> x + 1
    addOnePF::Int -> Int
    addOnePF = (+1)

    tensDigit :: (Integral a) => a -> a
    tensDigit x = mod 100 . div x $ 100

    roundTrip :: (Show a, Read a) => a -> a
    roundTrip a = read . show $ a

    incTimes::(Eq a , Num a) => a -> a -> a
    incTimes 0 n = n
    incTimes times n = 1 + (incTimes (times -1) n)
    

    applyTimes::(Eq a , Num a) => a -> (b -> b) -> b -> b
    applyTimes 0 f b = b
    applyTimes n f b = f (applyTimes (n-1) f b)



    main = do
        --print (0::Int)
        --print (add 1 0)
        --print (addOne 0)
        --print (addOnePF 0)
        --print ((addOnePF . addOnePF) 0)
        --print (tensDigit 1000000000)
        --print (roundTrip 4)
        print (incTimes 3 3) 
        print (applyTimes 3 (addOnePF) 0)

